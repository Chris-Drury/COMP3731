def CSRorCSC(R, C, V, n, nz):
    Rnew = [0] * nz
    # expand the R:
    i = 0
    p = 0
    l = 0
    while i < n:
        k = R[l]
        while p < k:
            Rnew[p] = i + 1
            p = p + 1
        l = l + 1
        i = i + 1
    R = Rnew
    # R is expanded into Rnew, sort R, C and V now by C
    i = 0
    while i < nz:
        min_idx = i
        j = i + 1
        while j < nz:
            if C[j] < C[min_idx]:
                min_idx = j
            j = j + 1

        tmpC = C[i]
        tmpR = R[i]
        tmpV = V[i]
        C[i] = C[min_idx]
        R[i] = R[min_idx]
        V[i] = V[min_idx]
        C[min_idx] = tmpC
        R[min_idx] = tmpR
        V[min_idx] = tmpV
        i = i + 1
    # Now compress C
    Cnew = [0] * n
    i = 0
    k = i
    while i < n:
        k = k + 1
        while k < nz and C[k - 1] == C[k]:
            k = k + 1

        Cnew[i] = k
        i = i + 1
    C = Cnew  
    # C is now in a compressed form
    return R, C, V

Row = [2,4,7,8]
Column = [1, 4, 2, 4, 1, 3, 4, 4]
Values = [1 ,4, 4, 8, 3, 9, 12, 16]
#[ 1, 0, 0, 4
#  0, 4, 0, 8
#  3, 0, 9, 12
#  0, 0, 0, 16]
print("CSR:")
print("Row:", Row)
print("Column:", Column)
print("Values: ", Values)

print("converted to CSC:")
Row, Column, Values = CSRorCSC(Row, Column, Values, len(Row), len(Values))
print("Column:", Column)
print("Row:", Row)
print("Values: ", Values)

print("back to the CSR:")
Column, Row, Values = CSRorCSC(Column, Row, Values, len(Column), len(Values))
print("Row:", Row)
print("Column:", Column)
print("Values: ", Values)

