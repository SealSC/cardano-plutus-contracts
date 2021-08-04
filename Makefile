up:
	@echo Startup playground
	@cd playground && docker-compose up -d
	@echo 'please open https://localhost:8009/'

down:
	@echo Shutdown playground
	@cd playground && docker-compose down
