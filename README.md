# Helm Charts

## Personal Helm charts

1. Create a secrets for ghrc repo, OCI config

```bash
kubectl create secret docker-registry <pull_secret_name> \
  --docker-server=ghcr.io \
  --docker-username=YOUR_GITHUB_USERNAME \
  --docker-password=YOUR_GITHUB_PAT \
  --docker-email=YOUR_EMAIL
```
For ocp link tthe secret for pull
```bash
    oc secrets link default <pull_secret_name> --for=pull
```

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
Modify your Helm values.yaml
```bash
env:
  - name: OCI_SHARED_CREDENTIALS_FILE
    value: /etc/oci/config

volumes:
  - name: oci-config-volume
    secret:
      secretName: oci-config

volumeMounts:
  - name: oci-config-volume
    mountPath: /etc/oci
    readOnly: true
```
Modify your Helm deployment.yaml (template)
```bash
spec:
  containers:
    - name: {{ .Chart.Name }}
      image: "{{ .Values.image.repository }}:{{ .Values.image.tag }}"
      env:
{{ toYaml .Values.env | indent 8 }}
      volumeMounts:
{{ toYaml .Values.volumeMounts | indent 8 }}
  volumes:
{{ toYaml .Values.volumes | indent 6 }}
```

2. Reference the secret in Helm Chart
```yaml
spec:
  template:
    spec:
      imagePullSecrets:
        - name: ghcr-secret
```
or in **values.yaml**
```yaml
imagePullSecrets:
  - name: ghcr-secret
```
then apply it in your template
```yaml
spec:
  template:
    spec:
      imagePullSecrets:
        {{- toYaml .Values.imagePullSecrets | nindent 8 }}
```

1. Create a secret for postgres datatbase.

```bash
kubectl create secret generic koku-postgres-secret --from-literal=password=<your_password>
```

1. Create a secret for minio.

```bash
kubectl create secret generic koku-minio-root-credentials --from-literal=username=<minio_root_username> --from-literal=password=<minio_root_password>
```
2. Install the chart to your cluster

```bash
helm install koku koku/koku
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
