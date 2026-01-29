from huggingface_hub import HfApi
api = HfApi()
models = api.list_models()
models = list(models)
print(len(models))
for x in models:
    print ("\t".join([x.id,str(x.downloads),str(x.likes),str(x)]))
    
