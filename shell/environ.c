#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "environ.h"

int comp(const char* s1, const char* s2)
{
  return strcmp(s1,s2) == 0;
}

variable* findByName(const char* name, variable* head)
{
  if (!name)
    return NULL;
  
  while (head)
  {
    if (comp(name, head->name))
      break;
    head = head->next;
  }
  return head;
}

variable* removeFromEnvironment(const char* name, variable* head)
{
  variable* tmp;
  if (!head)
    return NULL;
  if (!name || (*name == 0) )
    return head;
  
  if (comp(head->name,name))
  {
    tmp = head->next;
    free(head);
    return tmp;
  }
      
  head->next = removeFromEnvironment(name, head->next);
  return head;
}

variable* addToEnvironment(const char* name, const char* val, variable* head)
{
  if (!name || !val)
    return head;
  
  if (head == NULL)  
  {
    variable* newVar = (variable*) malloc( sizeof(variable) );
    strcpy(newVar->name, name);
    strcpy(newVar->val, val);
    newVar->next = NULL;
    head = newVar;
  }
  else
    head->next = addToEnvironment(name, val, head->next);
    
  return head;
}

void freeEnvironment(variable* head)
{
  if (!head)
    return;  
  freeEnvironment(head->next);
  free(head);
}