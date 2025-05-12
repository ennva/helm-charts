# Variables (can be overridden when running make)
RELEASE_NAME ?= koku
CHART_REPO ?= ennva/koku

.PHONY: help install-pipx install-precommit install-detect-secrets setup-precommit scan-secrets

help:
	@echo "Available targets:"
	@echo "  install-pipx            Install pipx safely"
	@echo "  install-precommit       Install pre-commit using pipx"
	@echo "  install-detect-secrets  Install detect-secrets using pipx"
	@echo "  setup-precommit         Setup pre-commit with detect-secrets hook"
	@echo "  scan-secrets            Scan repo and create .secrets.baseline"
	@echo "  helm-upgrade            Scan repo and create .secrets.baseline"

helm-repo-update:
	@echo "🔄 Updating Helm repositories..."
	helm repo update
	@echo "✅ Helm repositories updated."

# make helm-upgrade RELEASE_NAME=bam CHART_REPO=pippo/bam
helm-upgrade:
	@echo "🚀 Upgrading or installing release '$(RELEASE_NAME)' from chart '$(CHART_REPO)'..."
	helm upgrade --install $(RELEASE_NAME) $(CHART_REPO) --debug
	@echo "✅ Helm release upgraded or installed."

helm-deploy: helm-repo-update helm-upgrade
	@echo "🎯 Helm deploy (update + upgrade) completed."

install-pipx:
	@echo "🔧 Installing pipx..."
	sudo apt update
	sudo apt install -y pipx
	pipx ensurepath

install-precommit:
	@echo "🔧 Installing pre-commit via pipx..."
	pipx install pre-commit

install-detect-secrets:
	@echo "🔧 Installing detect-secrets via pipx..."
	pipx install detect-secrets

setup-precommit:
	@echo "⚙️  Setting up pre-commit with detect-secrets hook..."
	@printf "%s\n" "\
repos:\
  - repo: https://github.com/Yelp/detect-secrets\
    rev: v1.4.0\
    hooks:\
      - id: detect-secrets\
        args: ['--baseline', '.secrets.baseline']\
" > .pre-commit-config.yaml
	pre-commit install
	@echo "✅ Pre-commit installed and configured."

scan-secrets:
	@echo "🔍 Scanning for secrets and creating .secrets.baseline..."
	detect-secrets scan --all-files > .secrets.baseline
	@echo "✅ .secrets.baseline created. You can now commit it safely."
