def SparseAdd(R1, C1, V1, R2, C2, V2, R3, C3, V3, n, nz1, nz2, nz3):
    prev1 = 0
    prev2 = 0
    idx = 0
    idx2 = 0
    R3 = [0] * (nz3 * nz3)
    C3 = [0] * (nz3 + 1)
    V3 = [0] * (nz3 * nz3)
    while idx < n:
        upper1 = R1[idx]
        upper2 = R2[idx]
        j = prev1
        i = prev2
        while (prev1 <= j < upper1 or prev2 <= i < upper2):
            if j < upper1:
                col1 = C1[j]
            if i < upper2:
                col2 = C2[i]
            if j >= upper1:
                R3[idx2] = idx
                C3[idx2] = col2
                V3[idx2] = V2[i]
                idx2 = idx2 + 1
                i = i + 1
            elif i >= upper2:
                R3[idx2] = idx
                C3[idx2] = col1
                V3[idx2] = V1[j]
                idx2 = idx2 + 1
                j = j + 1
            elif col1 < col2:
                R3[idx2] = idx
                C3[idx2] = col1
                V3[idx2] = V1[j]
                idx2 = idx2 + 1
                j = j + 1
            elif col1 == col2:
                R3[idx2] = idx
                C3[idx2] = col1
                V3[idx2] = V1[j] + V2[i]
                idx2 = idx2 + 1
                j = j + 1
                i = i + 1
            elif col1 > col2:
                R3[idx2] = idx
                C3[idx2] = col2
                V3[idx2] = V2[i]
                idx2 = idx2 + 1
                i = i + 1

        prev1 = upper1
        prev2 = upper2
        idx = idx + 1
        
    return 1, 2, 3



# [0, 1, 0
#  1, 2, 3
#  0, 0, 10]
R1 = [1, 4, 5]
C1 = [2, 1, 2, 3, 3]
V1 = [1, 1, 2, 3, 10]

# [5, 2, 4
#  7, 0, 0
#  1, 0, 0]
R2 = [3, 4, 5]
C2 = [1, 2, 3, 1, 1]
V2 = [5, 2, 4, 7, 1]

R3 = [0] * 9
C3 = [0] * 9
V3 = [0] * 9

#result should be:
# [5, 3, 4
#  8, 2, 3
#  1, 0, 10]
R3, C3, V3 = SparseAdd(R1, C1, V1, R2, C2, V2, R3, C3, V3, len(R1), len(C1), len(C2), 9)


# [5, 6
#  0, 3]


# [0, 2
#  0, 1]


#result should be:
# [5, 8
#  0, 4]