// Implement a doubly-linked list in C

struct dbl_link_list_node {
  struct node *next;
  struct node *prev;
  char *val;
};

struct dbl_link_list {
  struct dbl_link_list_node *first_node;
  struct dbl_link_list_node *last_node;
}

int main(void) {

}
