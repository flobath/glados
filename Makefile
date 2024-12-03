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
	stack build
	ln -sf $$(stack path --local-install-root)/bin/$(STACK_TARGET)-exe $(TARGET)

clean:
	stack clean

fclean:
	stack purge

re: fclean all

tests_run:
	stack test --coverage

.PHONY: $(TARGET) clean fclean re tests_run
