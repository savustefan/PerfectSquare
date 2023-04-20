// Definim functia evenMagicSquare care primeste ca argument un intreg, size

let evenMagicSquare size =
    // Daca size este impar, afisam un mesaj de eroare 
    if size % 2 <> 0 then
        printfn "Size must be even."
        // Altfel, initializam o matrice de dimensiune size x size cu valoarea 0 
    else
        let magicSquare = Array2D.create size size 0
        // Stabilim pozitia initiala a numarului 1, la mijlocul primei linii
        let mutable i = size / 2
        let mutable j = size - 1
        // Parcurgem toate numerele de la 1 la sizesize si le plasam in matrice
        for num in 1..size*size do
            // Verificam daca pozitia actuala este deja ocupata
            if magicSquare.[i,j] <> 0 then
                // Daca pozitia este deja ocupata, trecem la pozitia din stanga jos
                j <- (j + size - 2) % size
                i <- (i + 1) % size
            // Plasam numarul la pozitia actuala
            magicSquare.[i,j] <- num
            // Trecem la pozitia din dreapta sus 
            j <- (j + 1) % size
            i <- (i + size - 1) % size
        // Afisam matricea rezultata
        printfn "Magic Square:"
        for i in 0..size-1 do
            for j in 0..size-1 do
                printf "%d\t" magicSquare.[i,j]
            printfn ""

evenMagicSquare 6