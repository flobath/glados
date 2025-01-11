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
	@ln -sf $$(stack path --local-install-root)/bin/$(STACK_TARGET)-exe $(TARGET)-exe
	@ln -sf $$(stack path --local-install-root)/bin/$(STACK_TARGET)-cmp $(TARGET)-cmp

clean:
	@stack clean

fclean:
	@stack purge

re: fclean all

tests_run: tests_unit_run tests_functional_run

tests_unit_run:
	@echo 'Running unit tests'
	@stack test --coverage

tests_functional_run: $(TARGET)
	@echo
	@echo 'Running functional tests'
	@echo
	@GLaDOS_EXEC_PATH="$$(realpath '$(TARGET)-exe')" \
	 TEST_CASES_DIRECTORY=./test/functional_tests/cases \
	 ./test/functional_tests/tester.sh

.PHONY: $(TARGET) all clean fclean re
.PHONY: tests_run tests_unit_run tests_functional_run
