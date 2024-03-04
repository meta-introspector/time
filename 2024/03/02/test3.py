from transformers import FunnelTokenizer, FunnelModel
tokenizer = FunnelTokenizer.from_pretrained("funnel-transformer/small")
model = FunneModel.from_pretrained("funnel-transformer/small")
text = "Replace me by any text you'd like."
encoded_input = tokenizer(text, return_tensors='pt')
output = model(**encoded_input)
