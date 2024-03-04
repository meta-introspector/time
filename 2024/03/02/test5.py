# Load model directly
from transformers import AutoTokenizer, AutoModelForCausalLM
import torch_xla.core.xla_model as xm
device = xm.xla_device()

tokenizer = AutoTokenizer.from_pretrained("microsoft/phi-1_5", trust_remote_code=True)

model = AutoModelForCausalLM.from_pretrained("microsoft/phi-1_5", trust_remote_code=True).to(device)
inputs = tokenizer.encode("def print_hello_world():", return_tensors="pt").to(device)
outputs = model.generate(inputs, max_new_tokens=100)
print(tokenizer.decode(outputs[0]))

