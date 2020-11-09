

#define GCMETA(STRUCT) struct STRUCT *forwarded;
#define GCINIT(V) ((V).forwarded = NULL)
