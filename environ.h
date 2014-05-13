#define MAX_VAR_NAME 128
#define MAX_VAR_VAL 1024

typedef struct variable
{
  char name[MAX_VAR_NAME];
  char val[MAX_VAR_VAL];
  struct variable *next;
} variable;

/* Adds a new variable with given name and val to a chain (linked list) of variables,
pointed by head. Returns the new head */
variable* addToEnvironment(const char* name, const char* val, variable* head);

/* Removes variables indicated by name from the variable chain.
Returns the new head*/
variable* removeFromEnvironment(const char* name, variable* head);

/* Releases all memory that was allocated for the chain */
void freeEnvironment(variable* head);

/* Finds a variable by its name. Returns NULL if not found */
variable* findByName(const char* name, variable* head);