##
## EPITECH PROJECT, 2022
## wolfram
## File description:
## Makefile
##

BINARY_PATH = $(shell stack path --local-install-root)

NAME = wolfram

all:
	stack build
	cp $(BINARY_PATH)/bin/wolfram-exe $(NAME)

clean:
	stack clean

fclean: clean
	rm -rf $(NAME)

re:	fclean all

.PHONY: all $(NAME) clean fclean re