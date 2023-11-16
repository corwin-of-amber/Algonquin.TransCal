#ifndef COLORS_H_KEMRW
#define COLORS_H_KEMRW


class Colors {
public:
    static const size_t MAX_COLORS = 16;
    typedef size_t color_index_t;

    enum cmp_t { NONE, SUP, EQ, SUB };

    typedef cmp_t Hierarchy[MAX_COLORS][MAX_COLORS];
};


#endif