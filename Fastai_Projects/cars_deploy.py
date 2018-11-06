#!/usr/bin/env python
# coding: utf-8

# In[1]:


from starlette.applications import Starlette
from starlette.responses import JSONResponse
import uvicorn
from fastai.vision import ImageDataBunch, create_cnn, open_image, get_transforms, models, data
import torch
import aiohttp
import asyncio
from io import BytesIO
import pandas as pd
from pathlib import Path

async def get_bytes(url):
    async with aiohttp.ClientSession() as session:
        async with session.get(url) as response:
            return await response.read()

app = Starlette()

@app.route('/')
async def homepage(request):
    return JSONResponse({'hello': 'world'})

@app.route("/classify-url", methods=["GET"])
async def classify_url(request):
    bytes = await get_bytes(request.query_params["url"])
    img = open_image(BytesIO(bytes))
    cars = Path('/home/jupyter/tutorials/data/Competitions/CarClassification/car_data')
    names = cars/'../names.csv'
    classes = pd.read_csv(names, names=['cars'], header=None)
    #data = ImageDataBunch.from_folder(cars, train = 'train', valid='test', ds_tfms=get_transforms(), size=224, bs=64)
    data_eval = ImageDataBunch.single_from_classes(cars, classes, tfms=get_transforms(), size=299)
    learn = create_cnn(data_eval, models.resnet50)
    learn.model.load_state_dict(torch.load(cars/'models/learn50-uf-10e.pth', map_location='cpu'))
    pred_class,class_pos,losses = learn.predict(img)
    return JSONResponse({
        "predicted_class": classes[153],
        "Probability": losses[1].sort(descending=True)[0]
        })

if __name__ == '__main__':
    uvicorn.run(app, host='0.0.0.0', port=8889)


# In[ ]:




