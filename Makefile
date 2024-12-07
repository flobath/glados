##
## EPITECH PROJECT, 2024
## glados
## File description:
## Makefile
##

TARGET		= glados
STACK_TARGET	= GLaDOS

all: $(TARGET)

$(TARGET):
	@stack build
	@ln -sf $$(stack path --local-install-root)/bin/$(STACK_TARGET)-exe $(TARGET)

clean:
	@stack clean

fclean:
	@stack purge

re: fclean all

tests_run: tests_run_unit tests_run_functional

tests_run_unit:
	@echo 'Running unit tests'
	@stack test

tests_run_functional: $(TARGET)
	@echo
	@echo 'Running functional tests'
	@GLaDOS_EXEC_PATH="$$(realpath '$(TARGET)')" \
	 TEST_CASES_DIRECTORY=./test/functional_tests/cases \
	 ./test/functional_tests/tester.sh

.PHONY: $(STACK_EXECUTABLE) $(TARGET) all clean fclean re tests_run
