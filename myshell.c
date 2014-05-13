#define PATH_SIZE 256
#define LINE_SIZE 2048
#define MAX_HISTORY 10
#define STDIN 0
#define STDOUT 1

#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include "LineParser.h"
#include <string.h>
#include "environ.h"
#include <sys/wait.h>
#include <stdlib.h>
#include <unistd.h>

struct command{
	char *line;
	struct command *next;
	struct command *prev;
};

struct history{
	int size;
	struct command *top;
	struct command *buttom;
};

struct child{
	pid_t cpid;
	struct child *next;
};

void myprint(cmdLine* pCmdLine){
	int arg = 1;
	while (arg < (pCmdLine->argCount)){
		printf("%s ", pCmdLine->arguments[arg]);
		arg++;
	}
	printf("\n"); 
}

void printHistory(struct history* stack){
	struct command *temp = stack->top;
	while (temp){
		printf("%s\n", temp->line);
		temp = temp->prev;
	}
}


void execute(cmdLine* pCmdLine){
	int flag;
	if (pCmdLine->inputRedirect != NULL){
		flag = close(0);
		if (flag == -1){
			perror(0);
			return;
		}
		flag = open(pCmdLine->inputRedirect, O_RDONLY);
		if (flag == -1){
			perror(0);
			return;
		}
	}
	if (pCmdLine->outputRedirect != NULL){
		flag = close(1);
		if (flag == -1){
			perror(0);
			return;
		}
		flag = creat(pCmdLine->outputRedirect, 0666);
		if (flag == -1){
			perror(0);
			return;
		}
	}
	
	if (pCmdLine->outputPipe != NULL){
		if (close(STDOUT) == -1){
			perror("error in child while closing standard output\n");
			return;
		}
		
		if (dup(pCmdLine->outputPipe[1]) == -1){
			perror("child failed duplicate write-end\n");
			return;
		}
		
		if (close(pCmdLine->outputPipe[1]) == -1){
			perror("child failed closing duplicated write-end\n");
			return;
		}
	}
	
	if (pCmdLine->inputPipe != NULL){
		if (close(STDIN) == -1){
			perror("error in child while closing standard input\n");
			return;
		}
		
		if (dup(pCmdLine->inputPipe[0]) == -1){
			perror("child failed duplicate read-end\n");
			return;
		}
		
		if (close(pCmdLine->inputPipe[0]) == -1){
			perror("child failed closing duplicated read-end\n");
			return;
		}
	}
	execvp(pCmdLine->arguments[0], pCmdLine->arguments);
	perror(0);	
}

void insertCmd(struct history* stack,struct command* cmd){
    if (stack->size >= MAX_HISTORY){
		struct command* temp = stack->buttom;
		stack->buttom = stack->buttom->next;
		stack->buttom->prev = NULL;
		free(temp->line);
		free(temp);
		(stack->size)--;
	}
	cmd->next = NULL;
	cmd->prev = NULL;
	if (stack->size == 0){
		stack->top = cmd;
		stack->buttom = cmd;
	}else{
		cmd->prev = stack->top;
		stack->top->next = cmd;
		stack->top = cmd;
	}
	(stack->size)++;
}

void deleteHistory(struct history* stack){
	struct command* temp;
	while ((stack->size) > 0){
		temp = (stack->top);
		stack->top = temp->prev;
		free(temp->line);
		free(temp);
		(stack->size)--;
	}
	free(stack);
}

	
void printenv(struct variable *varList){
	struct variable* current = varList;
	while (current){
		printf("%s = %s\n",current->name, current->val);
		current = current->next;
	}
}

void createPipes(cmdLine* cmdline){
	cmdLine* current = cmdline;
	while (current != NULL){
		if ((current->outputPipe!= NULL) && (current->next != NULL)){
			if (pipe(current->outputPipe) == -1){
				perror("error in createPipes\n");
				return;
			}
		}
		current = current->next;
	}
}

struct child *childBorn (struct child* children, int cpid){
	struct child *childborn = (struct child *)malloc(sizeof(struct child));
	childborn->cpid = cpid;
	childborn->next = 0;
	
	if (children == 0){
		return childborn;
	}
	struct child *current = children;
	while (current->next != 0){
		current = current->next;
	}
	current->next = childborn;
	return children;
}
	
int main (int argc , char* argv[], char* envp[]){
	struct variable *varList = 0;
	struct history *cmdStack = 0;
	struct command *tempcmd = 0;
	struct child *children = 0;
	cmdStack = malloc(sizeof(struct history));
	cmdStack->size = 0;
	cmdStack->top = NULL;
	cmdStack->top = NULL;
	int blocking = 0; 	
	pid_t cpid;
	int status;
	char line[LINE_SIZE];
	cmdLine *cmdline;
	
	while (1){
		char path[PATH_SIZE];
		if (!getcwd(path, PATH_SIZE)){
			printf("path name too long.");
			freeEnvironment(varList); 
			exit(EXIT_FAILURE);
		}
		printf("%s~>",path);
		fflush(stdout);

		fgets(line, LINE_SIZE, stdin);
		if (line[0] == '\n')
			continue;

		tempcmd = malloc(sizeof(struct command));
		(tempcmd->line) = malloc(sizeof(char)*strlen(line));
		strcpy(tempcmd->line, line);
		insertCmd(cmdStack, tempcmd);
		
		cmdline = parseCmdLines(line); 
		if (!cmdline){
			printf("problem with parsing.\n");
			deleteHistory(cmdStack);
			freeEnvironment(varList); 
			exit(EXIT_FAILURE);
		}
		
		createPipes(cmdline);
		
			int problem = 0;
			int i;
			for(i = 0 ; i < cmdline->argCount; i++){
				if (cmdline->arguments[i][0] == '$'){
					char* name = cmdline->arguments[i];
					struct variable* var;
					name++;
					var = findByName(name, varList);
					if (var){
						replaceCmdArg(cmdline, i, var->val);
					}else{
						perror("No Such Variable");
						freeCmdLines(cmdline);
						problem = 1;
						break;
					}
				}
			}

			if (problem)
				continue;

			if (strcmp(cmdline->arguments[0],"quit") == 0){
				printf("goodbye\n");
				deleteHistory(cmdStack);
				freeCmdLines(cmdline);
				freeEnvironment(varList);
				exit(EXIT_SUCCESS);
			}
			
			if (strcmp(cmdline->arguments[0],"set") == 0){
				varList = addToEnvironment(cmdline->arguments[1],cmdline->arguments[2],varList);
				freeCmdLines(cmdline);
				continue;
			}
			
			if (strcmp(cmdline->arguments[0],"delete") == 0){
				varList = removeFromEnvironment(cmdline->arguments[1],varList);
				freeCmdLines(cmdline);
				continue;
			}

			if ((strcmp(cmdline->arguments[0],"print") == 0) && (cmdline->argCount >= 2)){
				myprint(cmdline);
				freeCmdLines(cmdline);
				continue;
			}

			if (strcmp(cmdline->arguments[0],"printenv") == 0){
				printenv(varList);
				freeCmdLines(cmdline);
				continue;
			}

			if (strcmp(cmdline->arguments[0],"history") == 0){
				printHistory(cmdStack);
				freeCmdLines(cmdline);
				continue;
			} 	

			if ((strcmp(cmdline->arguments[0],"changedir") == 0) && (cmdline->argCount == 2)){
				if (chdir(cmdline->arguments[1]) < 0){
					printf("error in chdir");
					freeCmdLines(cmdline);
					deleteHistory(cmdStack);
					freeEnvironment(varList); 
					exit(EXIT_FAILURE);
				}
				freeCmdLines(cmdline);
				continue;
			}
		cmdLine* current = cmdline;
		while (current != NULL){
			if (!(cpid = fork())){
				execute(current);
				freeCmdLines(current);
				deleteHistory(cmdStack);
				freeEnvironment(varList); 
				exit(EXIT_FAILURE);
			}
			
			children = childBorn(children, cpid);
			if ((current->outputPipe != NULL) && (close(current->outputPipe[1]) == -1)){
				perror("parent failed closing write-end\n");
				exit(EXIT_FAILURE);
			}
			if ((current->inputPipe != NULL) && (close(current->inputPipe[0]) == -1)){
				perror("parent failed closing read-end\n");
				exit(EXIT_FAILURE);
			}
			
			if ((current->outputPipe == 0) && (current->blocking)){
				blocking = 1;
			}
			
			current = current->next;
		}
		while (children != 0){
			if ((children->next == 0) && (blocking) && (waitpid(children->cpid,&status,0) == -1)){
				printf("error in waitpid.\n");
				deleteHistory(cmdStack);
				freeEnvironment(varList); 
				exit(EXIT_FAILURE);
			}
			struct child* tempchild = children;
			children =  children->next;
			free(tempchild);
		}
		freeCmdLines(cmdline); 
	}
}		