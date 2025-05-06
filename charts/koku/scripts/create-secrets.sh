#!/bin/bash

# === CONFIGURATION ===
SECRET_NAME="ghcr-secret"
NAMESPACE="default"  # Change this if needed
GITHUB_USERNAME="your-github-username"
GITHUB_PAT="your-github-token"  # Better to pass this securely (see notes)
EMAIL="your-email@example.com"
POSTGRES_PWD="example"

# === CREATE SECRET ===
kubectl create secret docker-registry $SECRET_NAME \
  --docker-server=ghcr.io \
  --docker-username=$GITHUB_USERNAME \
  --docker-password=$GITHUB_PAT \
  --docker-email=$EMAIL \
  --namespace $NAMESPACE

echo -e "✅ Kubernetes imagePullSecret '$SECRET_NAME' created in namespace '$NAMESPACE'"

echo -e "\n------Linking docker secret fro image pull------"
oc secrets link default $SECRET_NAME --for=pull

echo -e "\n---Creating for postgres----"
kubectl create secret generic koku-postgres-secret --from-literal=password=$POSTGRES_PWD


