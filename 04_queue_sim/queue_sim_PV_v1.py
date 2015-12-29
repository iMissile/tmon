# -*- coding: utf-8 -*-
"""
Симуляция производительности многоуровневой клиент-серверной системы под
заданным профилем нагрузки на базе теории систем массового обслуживания

Created on Mon Dec 21 13:22:36 2015

@author: vpanfilov
"""

# Event discrete simulation for Python
import simpy
# Python Data Analysis Library (таблицы данных и методы их обработки)
import pandas as pd
# Для случайных величин из экспоненциального распределения
from random import expovariate
# Итераторы (для метода Round-robin)
import itertools
import numpy as np
import os
import time


servers = [["Web Server 1"], ["App Server 1", "App Server 2", "App Server 3"],
           ["DB Server 1", "DB Server 2"], ["S1", "S2", "S3", "S4"]]
service_time = [[0.035], [0.03, 0.03, 0.25], [0.06, 0.065],
                [0.25, 0.04, 0.04, 0.04]]

# меняем текущую директорию на папку со скриптом
abspath = os.path.abspath(__file__)
dname = os.path.dirname(abspath)
os.chdir(dname)
# Переходим в папку с исходными данными
os.chdir("../90_load_profile_data/")
# Исходные данные по профилю нагрузки
input_file = "LoadProfile_final.csv"

# Читаем таблицу с профилем нагрузки из CSV
arrival_rate = pd.read_csv(input_file)
# Парсим строки в столбце timestamp во временной формат
arrival_rate["timestamp"] = pd.to_datetime(arrival_rate["timestamp"])
# Масштабируем профиль нагрузки (*270/90/8)
arrival_rate["value"] = arrival_rate["value"] * 0.375
# Определяем время начала симуляции
beginTime = min(arrival_rate["timestamp"])
# Будем моделировать 5 недель
num_weeks = 5
endTime = beginTime + pd.Timedelta(weeks = num_weeks)
# Считаем количество минут в симуляции
simTime = (endTime- beginTime).days * 24 * 60
# Индексирем по времени
arrival_rate.set_index("timestamp", inplace = True)

arr_data = pd.DataFrame()
for i in range(num_weeks):
    tmp = pd.DataFrame(arrival_rate.value)
    tmp.index = arrival_rate.index + pd.Timedelta(weeks = i)
    arr_data = pd.concat([arr_data, tmp])
    
arrival_rate = arr_data

'''
def ApplyToServers(function, servers):
    result = []
    for tier in range(len(servers)):
        r = list(map(function, servers[tier]))
        result.append(r)
    return(result)
'''
    
# Создаём ресурсы Simpy для всей структуры серверов
def MakeSimpyResource(servers, env):
    result = []
    for tier in range(len(servers)):
        r = list(map(lambda x: simpy.Resource(env), servers[tier]))
        result.append(r)
    return(result)

def RoundRobin(n):
    # Возвращаем итератор, кторый циклически повторяет значения от 1 до n
    return itertools.cycle(range(n))
    
def MinQueue(tier):
    # Рассчитываем длину очереди для каждого сервера на уровне tier
    queue_count = map(lambda r: r.count + len(r.queue), server_tiers[tier])
    # возвращаем номер сервера с минимальной очередью
    yield(np.argmin(list(queue_count)))

# Делаем итераторы по серверам для каждого уровня
RR = []
for i in range(len(servers)):
    RR.append(RoundRobin(len(servers[i])))

# генерируем запросы к серверу друг за другом, базируясь на профиле нагрузки
# (время, через которое придёт следующий запрос берётся из экспоненциального
# распределения с соответствующем параметром интенсивности λ)
def source(env, data):
    number = 0
    while True:
        # ожидаем новый запрос
        # определяем текущее время процесса симуляции
        currtime = beginTime + pd.Timedelta(minutes = env.now)
        # находим текущую интенсивность входящих запросов (по ближайшему
        # предыдущему значению в профиле нагрузки)
        arr_rate = arrival_rate["value"].asof(currtime)
        # время поступления следующего запроса
        next_arr_time = expovariate(arr_rate)
        yield env.timeout(next_arr_time)
        # новый запрос поступил
        number += 1
        # обрабатываем запрос
        req = server_request(env, number, data)
        env.process(req)
        

def server_request(env, num, data):
    for tier in range(len(servers)):
        # выбираем номер сервера на нужном уровне с помощью RoundRobin
        server_num = next(RR[tier])
        # выбираем номер сервера на нужном уровне по минимальной очереди
        # server_num = next(MinQueue(tier))
        with server_tiers[tier][server_num].request() as req:
            # время поступления запроса
            arrive = env.now
            # запрашиваем нужный сервер
            yield req
            # время ожидания начала обработки запроса
            wait = env.now - arrive
            # случайное время обработки запроса (эксп. распределение)
            service = expovariate(1.0 / service_time[tier][server_num])
            # чило запросов в очереди
            queue = len(server_tiers[tier][server_num].queue)
            # записываем всю информацию об обработке запроса на сервере
            info = [num, tier, server_num, arrive, wait, service, queue]
            data.append(info)
            yield env.timeout(service)
    
print("Начинаем симуляцию.", flush = True)
# Засекаем время
t = time.process_time()
#Данные симуляции будем сохранять в списрк
data = []

env = simpy.Environment()
# Создаём ресурсы SimPy для каждого сервера
server_tiers = MakeSimpyResource(servers, env)
# Определяем процесс генерации запросов
env.process(source(env, data))
# Начинаем симуляцию
env.run(until = simTime)
sim_elapsed_time = round(time.process_time() - t, 1)
print("Симуляция заняла", sim_elapsed_time, "секунд")

# Преобразовываем в формат Pandas
df = pd.DataFrame(data)
df.columns = ["num", "tier", "server_num", "arrive", "wait", "service", "queue"]
#df = df.reindex(columns = ["num", "tier", "server_num", "arrive", "wait", "service"])

'''
# Суммируем время ожидания и время выполения каждого запроса по всем уровням
resp_time_df = df[["wait", "service"]].groupby(df["num"]).sum()
# И рассчитываем общее время отклика
resp_time_df["response"] = resp_time_df["wait"] + resp_time_df["service"]
'''

resp_time_df = df.groupby("num")
resp_time_df = resp_time_df.agg({"arrive": "min",
                                 "wait": "sum",
                                 "service": "sum"})
resp_time_df["num"] = resp_time_df.index
resp_time_df = resp_time_df.assign(response = resp_time_df.wait + resp_time_df.service)
resp_time_df["timestamp"] = pd.to_timedelta(resp_time_df.arrive, "m") + beginTime
resp_time_df.set_index("timestamp", inplace = True)
resp_time_df = resp_time_df[["num", "wait", "service", "response"]]

print("Среднее время отклика:")
print(resp_time_df.response.mean())
print("Максимальное время отклика:")
print(resp_time_df.response.max())
print("Минимальное время отклика:")
print(resp_time_df.response.min())

# Среднее время отклика по пятиминутным интервалам
resp_time_bin_df = (resp_time_df[["wait", "service", "response"]]
                    .resample("5T", how="mean"))
                    