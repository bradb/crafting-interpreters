#include "linked_list.h"
#include <stdio.h>
#include <stdlib.h>

struct list_node {
   struct list_node *prev, *next;
   ll_data_t data;
};

struct list {
   struct list_node *first, *last;
};

// constructs a new (empty) list
struct list *list_create(void) {
  struct list *lst = malloc(sizeof (struct list));
  lst->first = NULL;
  lst->last = NULL;

  return lst;
}

// counts the items on a list
size_t list_count(const struct list *list) {
  size_t n = 0;

  for (struct list_node *node = list->first; node != NULL; n++) {
    node = node->next;
  }

  return n;
}

// inserts item at back of a list
void list_push(struct list *list, ll_data_t item_data) {
  struct list_node *node = malloc(sizeof (struct list_node));
  node->next = NULL;
  node->prev = NULL;
  node->data = item_data;

  if (list->last == NULL) {
    list->first = node;
    list->last = node;
  } else {
    node->prev = list->last;
    list->last->next = node;
    list->last = node;
  }
}

// removes item from back of a list
ll_data_t list_pop(struct list *list) {

  if (list->last != NULL) {
    ll_data_t val = list->last->data;

    list->last = list->last->prev;
    if (list->last != NULL) {
      list->last->next = NULL;
    } else {
      list->first = NULL;
    }
    return val;
  } else {
    return 0;
  }
}

// inserts item at front of a list
void list_unshift(struct list *list, ll_data_t item_data) {
  struct list_node *node = malloc(sizeof (struct list_node));
  node->prev = NULL;
  node->next = NULL;
  node->data = item_data;

  if (list->first == NULL) {
    list->first = node;
  } else {
    node->next = list->first;
    list->first->prev = node;
    list->first = node;
  }
}

// removes item from front of a list
ll_data_t list_shift(struct list *list) {
  if (list->first != NULL) {
    ll_data_t val = list->first->data;

    list->first = list->first->next;
    if (list->first != NULL) {
      list->first->prev = NULL;
    } else {
      list->last = NULL;
    }

    return val;
  } else {
    return 0;
  }
}

// deletes a node that holds the matching data
void list_delete(struct list *list, ll_data_t data) {
  for (struct list_node *node = list->first; node != NULL; node = node->next) {
    if (node->data == data) {
      if (node->prev != NULL) {
        node->prev->next = node->next;
      } else {
        list->first = node->next;
        if (list->first != NULL) {
          list->first->prev = NULL;
        }
      }

      if (node->next != NULL) {
        node->next->prev = node->prev;
      } else {
        list->last = node->prev;
        if (list->last != NULL) {
          list->last->next = NULL;
        }
      }

      free(node);

      return;
    }
  }
}

// destroys an entire list
// list will be a dangling pointer after calling this method on it
void list_destroy(struct list *list) {
  for (struct list_node *node = list->first; node != NULL; ) {
    struct list_node *next_node = node->next;
    free(node);
    node = next_node;
  }

  free(list);
}
