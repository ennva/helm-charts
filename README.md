# Helm Charts

## Personal Helm charts

### <u>Prerequisites</u>

**Create a secrets**

- **For ghrc repo**

```bash
kubectl create secret docker-registry <pull_secret_name> \
  --docker-server=ghcr.io \
  --docker-username=YOUR_GITHUB_USERNAME \
  --docker-password=YOUR_GITHUB_PAT \
  --docker-email=YOUR_EMAIL
```
If ocp, link the secret for pull
```bash
    oc secrets link default <pull_secret_name> --for=pull
```
Reference the secret in Helm Chart
```yaml
spec:
  template:
    spec:
      imagePullSecrets:
        - name: ghcr-secret
```
- **For OCI config**

Suppose your ~/.oci/config file looks like this:
```ini
[DEFAULT]
user=ocid1.user.oc1..example
fingerprint=xx:xx:xx
key_file=/path/to/oci_api_key.pem
tenancy=ocid1.tenancy.oc1..example
region=us-ashburn-1
```
You can create a Kubernetes Secret like this:
```bash
kubectl create secret generic oci-config \
  --from-file=oci_config=/path/to/oci_config \
  --from-file=oci_api_key.pem=/path/to/oci_api_key.pem
```


- **For postgres database**

```bash
kubectl create secret generic koku-postgres-secret --from-literal=user=<your_user> --from-literal=password=<your_password>
```

- **For minio.**

```bash
kubectl create secret generic koku-minio-root-credentials --from-literal=username=<minio_root_username> --from-literal=password=<minio_root_password>
```
2. Install the chart to your cluster

```bash
helm install koku koku/koku
```

---

### ✅ How to generate a real `oci_api_key.pem`

If you want to generate it securely yourself, run:

```bash
openssl genrsa -out oci_api_key.pem 2048
```

And to get the corresponding public key (used in your OCI user config):

```bash
openssl rsa -pubout -in oci_api_key.pem -out oci_api_key_public.pem
```
---
A sample `oci_api_key.pem` is simply an RSA **private key** in PEM format. You can generate one locally using OpenSSL, but here’s an example of what the file *looks like*:

---

### ✅ Example: `oci_api_key.pem` (sample RSA private key)

```pem
-----BEGIN RSA PRIVATE KEY-----
MIIEpAIBAAKCAQEArqGdRZ9ZcFoJSBvWArCn1n6c9r3JDjzMGFYqPfqKH2T4AxO8
uVtqTukGV3onvQZ9sGKMxvhGJQk8+0aKjvKwAJKeAnjMXC2Zrp3Kj7WTsCMfHe+0
R5Vkv0nZme9+hQlR2JypxX1sTfGmAkgMRBxSJoUFvQ0VwXEM6Pf9aOMJq8JTTZp5
ShxKavGPR8yQ3Yq5LZgZ6SnJPjIHLNEgJgz7Jz6q/4QEyOIEiyk9KhYzTzJxS5Sl
tB7b6FycsBfCE2ug1U1RMdWfnBHFIE+TxB9U7La7BPOea+MLQ9cK+iFn3wPgI3L5
oCH23J2iBNgVYz2lJzq6hEhD2umFdFlSBiVqZQIDAQABAoIBAGPvKkcmvOIt/ywU
UasDdKPonfuPi7hDXwJg6jl6eePuwkWyLa3WABnGj2UMD4RGKzV3z7GT2EF/XYHy
fQ6BpiWaz0BrzdUIRLOITeA8OgYp82oJkjIMJPQ20Ft3jbrYOAz/8NGHb1E0rAJJ
f6H5N1yo3N3Z5PdzT5cxMhaNiwM0+7OM9EepRG7M3bwXQFznLDG5zswU5T9N9gQZ
...
-----END RSA PRIVATE KEY-----
```

> ⚠️ **This is only a dummy/sample**. Never use a public example key in real environments.

---

### Add remote repo locally

```bash
helm repo add ennva-koku https://ennva.github.io/helm-charts
```
---

## Configure Git pre-commit

You can **add a `pre-commit` hook** to block commits that contain secrets, credentials, or sensitive patterns by using the **[pre-commit](https://pre-commit.com/)** framework together with some specialized hooks like:

* [detect-secrets](https://github.com/Yelp/detect-secrets)
* [git-secrets](https://github.com/awslabs/git-secrets)
* Built-in regex hooks

---

## ✅ Step-by-step guide to block secrets in commits

### 1. Install `pre-commit`

```bash
pip install pre-commit
```

---

### 2. Create `.pre-commit-config.yaml` in your repository root

Here’s a sample configuration using `detect-secrets`:

```yaml
repos:
  - repo: https://github.com/Yelp/detect-secrets
    rev: v1.4.0
    hooks:
      - id: detect-secrets-hook
```

### Alternative: Use simple regex for blocking patterns (quick start)

```yaml
repos:
  - repo: https://github.com/pre-commit/pre-commit-hooks
    rev: v4.5.0
    hooks:
      - id: detect-aws-credentials
      - id: detect-private-key
      - id: detect-secrets
  - repo: local
    hooks:
      - id: block-common-patterns
        name: Block common secret patterns
        entry: grep -E 'AKIA[0-9A-Z]{16}|-----BEGIN PRIVATE KEY-----|password\s*='
        language: system
        types: [text]
```

---

### 3. Install the hooks into your local git repo

```bash
pre-commit install
```

This will automatically install the `pre-commit` hook into `.git/hooks/pre-commit`.

---

### 4. (Optional) Scan existing code base

```bash
pre-commit run --all-files
```

---

### 5. Add `.pre-commit-config.yaml` to your repository and commit it (after checking it doesn’t contain secrets!).

---

## ✅ Best practices:

| Tool                     | Use case                                      |
| ------------------------ | --------------------------------------------- |
| `detect-secrets` (Yelp)  | Advanced, supports baselining, entropy checks |
| `git-secrets` (AWS Labs) | Good for AWS keys, customizable regex         |
| `pre-commit-hooks`       | Built-in basic AWS key, private key detectors |

---

## 🚀 Recommended combo:

For **strong protection**, I recommend combining both:

1. **Yelp detect-secrets (baseline support)**
2. **AWS git-secrets (installed manually)**
3. **Custom regex in pre-commit-config.yaml**

---

Would you also like a **pre-configured `detect-secrets` baseline + pre-commit that blocks commits if new secrets are found?**
If yes, just say "**yes, baseline example.**"
