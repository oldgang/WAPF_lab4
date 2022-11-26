type ZamowienieUslugi = {lokalizacja:string; predkoscOczekiwana:int; predkoscMinimalna:int; maksymalnyAbonament:int}

type ZamowieniePrzyjete = {id:int; zamowienie:ZamowienieUslugi}

type ZamowieniePrzetworzone = {id:int; odleglosc:float; zamowienie:ZamowienieUslugi}

type DzialaniePotwierdzone = {id:int; predkoscRzeczywista:int; zamowienie:ZamowienieUslugi;}

type Oferta = {id:int; predkosc:int; abonament:int; zamowienie:ZamowienieUslugi}

type UmowaPodpisana = {id: int; predkosc:int; abonament:int}

type Result<'T,'TError> =
    | Ok of ResultValue:'T
    | Error of ErrorValue:'TError


// przyjęcie zamówienia i nadanie mu losowego id, zawsze ok
let przyjecieZamowienia (z: ZamowienieUslugi) =
    let r = System.Random()
    Ok {id = r.Next(10000); zamowienie = z}

// sprawdzenie czy pole lokalizacja nie jest puste i wyznaczenie odległości od infrastruktury (pseudolosowo)
let walidacjaZamowienia (z: ZamowieniePrzyjete) =
    let r = System.Random()
    match z.zamowienie.lokalizacja with
    | "" -> Error "Nie podano lokalizacji"
    | _ -> 
        match z.zamowienie.maksymalnyAbonament with
        | 0 -> Error "Nie podano maksymalnej kwoty abonamentu"
        | _ -> Ok {id = z.id; zamowienie = z.zamowienie; odleglosc = System.Math.Round(r.NextDouble()*10.0, 2)}

// weryfikacja dostępności usługi na podstawie lokalizacji i odległości oraz określenie rzeczywiście osiągalnych prędkości usługi
let weryfikacjaWarunkowTechnicznych (z: ZamowieniePrzetworzone) = 
    match z.zamowienie.lokalizacja with
    | l when System.String.Equals(l, "Gdynia") || System.String.Equals(l, "Gdańsk")-> 
        match z.odleglosc with
        | o when o > 5 && z.zamowienie.lokalizacja="Gdańsk"-> Error $"Zbyt duża odległość od infrastruktury w lokalizacji {z.zamowienie.lokalizacja}: {o}km"
        | o when o > 8 -> Ok {id = z.id; zamowienie = z.zamowienie; predkoscRzeczywista = 40}
        | _ -> Ok {id = z.id; zamowienie = z.zamowienie; predkoscRzeczywista = z.zamowienie.predkoscOczekiwana}
    | _ -> Error $"Brak dostępności usług w tej lokalizacji: {z.zamowienie.lokalizacja}"

// utworzenie oferty dla klienta
let utworzenieOferty (z:DzialaniePotwierdzone) =
    let parametryUslugi maksymalnaCena predkosc = 
        match predkosc with
        | 10 when maksymalnaCena < 25 -> (10, 25);
        | 20 when maksymalnaCena < 40 -> (20, 40);
        | 40 when maksymalnaCena < 60 -> (40, 60);
        | 60 when maksymalnaCena < 80 -> (60, 80);
        | 100 when maksymalnaCena < 115 -> (100, 115);
        | x when maksymalnaCena < int (float x*1.5) -> (int (float x*1.5), x) ;
        | _ -> (int(float maksymalnaCena*(2.0/3.0)), maksymalnaCena)
    
    if(z.predkoscRzeczywista < z.zamowienie.predkoscMinimalna) then
        Error "Rzeczywista prędkość mniejsza niż minimalna akceptowalna, nie utworzono oferty"
    else
        let parametry = parametryUslugi z.zamowienie.maksymalnyAbonament z.predkoscRzeczywista
        let predkosc = fst parametry
        let abonament = snd parametry
        Ok {id = z.id; predkosc = predkosc; abonament = abonament; zamowienie = z.zamowienie}
    
let podpisanieUmowy (z: Oferta) =
    Ok {id = z.id; predkosc = z.predkosc; abonament = z.abonament}

let bind f = 
    fun res ->
        match res with 
        | Ok z -> f z
        | Error e -> Error e

//Zad 1a
let validateRequest = 
    przyjecieZamowienia
    >> bind walidacjaZamowienia
    >> bind weryfikacjaWarunkowTechnicznych
    >> bind utworzenieOferty
    >> bind podpisanieUmowy

let test() = 
    let zamowienie1 = {lokalizacja = "Gdynia"; predkoscOczekiwana = 300; predkoscMinimalna=80; maksymalnyAbonament = 650}
    let zamowienie2 = {lokalizacja = "Gdynia"; predkoscOczekiwana = 100; predkoscMinimalna=80; maksymalnyAbonament = 60}
    let zamowienie3 = {lokalizacja = "Gdańsk"; predkoscOczekiwana = 0; predkoscMinimalna=0; maksymalnyAbonament = 15}
    let zamowienie4 = {lokalizacja = "Kartuzy"; predkoscOczekiwana = 60; predkoscMinimalna=20; maksymalnyAbonament = 25}
    let zamowienie5 = {lokalizacja = ""; predkoscOczekiwana = 20; predkoscMinimalna=10; maksymalnyAbonament = 15}
    printfn $"\n\n{validateRequest zamowienie1}"
    printfn $"{validateRequest zamowienie2}"
    printfn $"{validateRequest zamowienie3}"
    printfn $"{validateRequest zamowienie4}"
    printfn $"{validateRequest zamowienie5}\n\n"

test()