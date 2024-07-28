import re
import torch
from torch import nn
from torch import optim
from torch.utils.data import DataLoader, TensorDataset
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from collections import defaultdict
import test3

# Tokenization
def tokenize_mes(code):
    """Basic tokenization for MES"""
    tokens = re.findall(r'\(|\)|[^\s()]+', code)
    return tokens


def create_vocabulary(all_tokens):
    """ Create vocabulary"""
    return sorted(set(all_tokens))


def tokens_to_indices(tokens, vocab):
    """Convert tokens to indices"""
    return [vocab.index(token) for token in tokens]


class EmbeddingAutoencoder(nn.Module):
    """Autoencoder for embedding compression"""
    def __init__(self, vocab_size, embedding_dim, compressed_dim):
        super().__init__()
        self.embedding = nn.Embedding(vocab_size, embedding_dim)
        self.encoder = nn.Sequential(
            nn.Linear(embedding_dim, compressed_dim),
            nn.ReLU()
        )
        self.decoder = nn.Sequential(
            nn.Linear(compressed_dim, embedding_dim),
            nn.ReLU()
        )

    def forward(self, input_x):
        """forward function"""
        embedded = self.embedding(input_x)
        encoded = self.encoder(embedded)
        decoded = self.decoder(encoded)
        return decoded


best = 100

def process_mes_code(mes_code, embedding_dim=28,
                     compressed_dim=17, epochs=1000):
    """Example usage"""
    # Tokenize
    tokens = tokenize_mes(mes_code)
    print("tokens",len(tokens))
    # Create vocabulary
    vocab = create_vocabulary(tokens)
    vocab_size = len(vocab)
    print("vocab",vocab_size)

    # Convert to indices
    indices = tokens_to_indices(tokens, vocab)

    print("indicies",len(indices))
        
    # Prepare data for PyTorch
    data = torch.tensor(indices)
    np.save("data", data)
    dataset = TensorDataset(data)
    np.save("dataset", dataset)
    dataloader = DataLoader(dataset, batch_size=32, shuffle=True)

    # Initialize autoencoder
    model = EmbeddingAutoencoder(vocab_size, embedding_dim, compressed_dim)
    criterion = nn.MSELoss()
    optimizer = optim.Adam(model.parameters())

    # Train
    epoch = 0
    for epoch in range(epochs):
        for batch in dataloader:
            inputs = batch[0]
            outputs = model(inputs)
            loss = criterion(outputs, model.embedding(inputs))
            optimizer.zero_grad()
            loss.backward()
            optimizer.step()

        if (epoch + 1) % 100 == 0:
            print(f'Epoch [{epoch+1}/{epochs}], Loss: {loss.item():.4f}')

    global best
    if loss.item() < best:
        best = loss.item()
        print("Embedding",embedding_dim,    "Compressed", compressed_dim, "Loss",loss.item())

    return model, vocab


def save_first_embedding(model, file_path):
    """Save the weights of the first embedding layer."""
    embedding_weights = model.embedding.weight.detach().cpu().numpy()
    np.save(file_path, embedding_weights)
    np.savetxt(file_path+".txt", embedding_weights)
    print(f"First embedding layer saved to {file_path}")


def load_first_embedding(file_path):
    """Load the saved weights of the first embedding layer."""
    return np.load(file_path)


def print_token_embeddings(model, vocab):
    """Print the entire embedding for each token."""
    embedding_weights = model.embedding.weight.detach().cpu().numpy()
    
    for token, embedding in zip(vocab, embedding_weights):
        print(f"\nToken: '{token}'")
        print("Embedding:")
        print(embedding)
        print("Embedding shape:", embedding.shape)
        print("L2 norm:", np.linalg.norm(embedding))
        print("-" * 50)
        
def token_value_report(model, vocab):
    """Produce a report on the value of each token."""
    embedding_weights = model.embedding.weight.detach().cpu().numpy()

    # Calculate the L2 norm of each token's embedding
    token_norms = np.linalg.norm(embedding_weights, axis=1)

    # Calculate the cosine similarity between each pair of tokens
    similarity_matrix = embedding_weights @ embedding_weights.T
    norms = np.linalg.norm(embedding_weights, axis=1, keepdims=True)
    similarity_matrix /= norms
    similarity_matrix /= norms.T

    # Find the most similar tokens for each token
    most_similar = defaultdict(list)
    for i, token in enumerate(vocab):
        similarities = similarity_matrix[i]
        most_similar_indices = np.argsort(similarities)[-6:-1]
        # Top 5 similar tokens (excluding self)
        most_similar[token] = [(vocab[idx], similarities[idx]) for idx in
                               most_similar_indices[::-1]]

    # Create a DataFrame with the results
    df = pd.DataFrame({
        'Token': vocab,
        'Embedding Norm': token_norms,
        'Most Similar Tokens': [most_similar[token] for token in vocab]
    })

    # Sort by embedding norm (you could change this to sort by a different
    # metric if desired)
    df = df.sort_values('Embedding Norm', ascending=False).reset_index(
        drop=True)

    return df


def visualize_token_distribution(df):
    """Visualize the distribution of token embedding norms."""
    plt.figure(figsize=(10, 6))
    plt.hist(df['Embedding Norm'], bins=30)
    plt.title('Distribution of Token Embedding Norms')
    plt.xlabel('Embedding Norm')
    plt.ylabel('Frequency')
    plt.savefig('token_norm_distribution.png')
    plt.close()


def analyze_mes_embeddings(model, vocab, save_path='mes_first_embedding.npy'):
    # Save the first embedding layer
    save_first_embedding(model, save_path)

    # Produce token value report
    report = token_value_report(model, vocab)
    print_token_embeddings(model, vocab)

    # Save report to CSV
    report.to_csv('token_value_report.csv', index=False)
    print("Token value report saved to token_value_report.csv")

    # Visualize token distribution
    visualize_token_distribution(report)
    print("Token norm distribution plot saved to token_norm_distribution.png")

    # Print top 10 tokens by embedding norm
    print("\nTop 10 tokens by embedding norm:")
    print(report[['Token', 'Embedding Norm']].head(10).to_string(index=False))

    # Print example of similar tokens
    print("\nExample of similar tokens:")
    example_token = report['Token'].iloc[0]
    print(f"Tokens most similar to '{example_token}':")
    for token, similarity in report['Most Similar Tokens'].iloc[0]:
        print(f"  {token}: {similarity:.4f}")


        
def main():
    """Driver"""

    """Example MES code (this is a simplified example)"""
    #2024/05/30/mes
    mes_code = "\n".join(test3.load())
    print("input size:", len(mes_code))
    # Process the code
    mes_model, vocab = process_mes_code(mes_code)
    

#    for embedding_dim in range(24,45,):
#        for compressed_dim in range(1,embedding_dim):
#            process_mes_code(mes_code,
#                             embedding_dim=embedding_dim,
#                             compressed_dim=compressed_dim,
#                             epochs=1000)
            

    print("Compressed embedding size:",
          mes_model.encoder[0].out_features)

    analyze_mes_embeddings(mes_model, vocab)


main()
