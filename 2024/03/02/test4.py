# pip install -q transformers
import torch
from transformers import AutoModelForCausalLM, AutoTokenizer

checkpoint = "Deci/DeciCoder-1b"
#device = "cuda" # for GPU usage or "cpu" for CPU usage
import torch_xla.core.xla_model as xm
device = xm.xla_device()
tokenizer = AutoTokenizer.from_pretrained(checkpoint)
model = AutoModelForCausalLM.from_pretrained(checkpoint, torch_dtype=torch.bfloat16, trust_remote_code=True).to(device)

inputs = tokenizer.encode("def print_hello_world():", return_tensors="pt").to(device)
outputs = model.generate(inputs, max_new_tokens=100)
print(tokenizer.decode(outputs[0]))

