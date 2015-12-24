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
import os
import time


servers = [["Web Server 1"], ["App Server 1", "App Server 2"], ["Database Server 1"]]
service_time = [[0.03], [0.05, 0.04], [0.05]]

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
endTime = beginTime + pd.Timedelta(weeks = 5)
# Считаем количество минут в симуляции
simTime = (endTime- beginTime).days * 24 * 60
# Индексирем по времени
arrival_rate.set_index("timestamp", inplace = True)

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
        with server_tiers[tier][server_num].request() as req:
            # время поступления запроса
            arrive = env.now
            # запрашиваем нужный сервер
            yield req
            # время ожидания начала обработки запроса
            wait = env.now - arrive
            # случайное время обработки запроса (эксп. распределение)
            service = expovariate(1.0 / service_time[tier][server_num])
            # записываем всю информацию об обработке запроса на сервере
            info = [num, tier, server_num, arrive, wait, service]
            data.append(info)
            yield env.timeout(service)
    
print("Начинаем симуляцию.", flush = True)
t = time.process_time()
data = []
env = simpy.Environment()
server_tiers = MakeSimpyResource(servers, env)
env.process(source(env, data))
env.run(until = simTime)
sim_elapsed_time = round(time.process_time() - t, 1)
print("Симуляция заняла", sim_elapsed_time, "секунд")


df = pd.DataFrame(data)
df.columns = ["num", "tier", "server_num", "arrive", "wait", "service"]
#df = df.reindex(columns = ["num", "tier", "server_num", "arrive", "wait", "service"])

resp_time_df = df[["wait", "service"]].groupby(df["num"]).sum()
resp_time_df["response"] = resp_time_df["wait"] + resp_time_df["service"]

print("Среднее время отклика:")
print(resp_time_df.response.mean())
print("Максимальное время отклика:")
print(resp_time_df.response.max())
print("Минимальное время отклика:")
print(resp_time_df.response.min())