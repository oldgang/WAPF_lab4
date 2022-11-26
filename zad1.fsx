//kolejność typów zgodna z przepływem
type ZamowienieUslugi = {lokalizacja:string; predkoscOczekiwana:int; predkoscMinimalna:int; maksymalnaCena:int}

type ZamowieniePrzyjete = {id:int; zamowienie:ZamowienieUslugi}

type ZamowieniePrzetworzone = {id:int; odleglosc:float; zamowienie:ZamowienieUslugi}

type DzialaniePotwierdzone = {id:int; predkoscRzeczywista:int; zamowienie:ZamowienieUslugi;}

type Oferta = {id:int; predkosc:int; abonament:int; zamowienie:ZamowienieUslugi}

type UmowaPodpisana = {id: int; predkosc:int; abonament:int}

type Result<'T,'TError> =
    | Success of ResultValue:'T
    | Error of ErrorValue:'TError


// przyjęcie zamówienia i nadanie mu losowego id, zawsze ok
let przyjecieZamowienia (z: ZamowienieUslugi) =
    let r = System.Random()
    Success {id = r.Next(10000); zamowienie = z}

// sprawdzenie czy pole lokalizacja nie jest puste i wyznaczenie odległości od infrastruktury (pseudolosowo)
let walidacjaZamowienia (z: ZamowieniePrzyjete) =
    let r = System.Random()
    match z.zamowienie.lokalizacja with
    | "" -> Error "Nie podano lokalizacji"
    | _ -> 
        match z.zamowienie.maksymalnaCena with
        | 0 -> Error "Nie podano maksymalnej kwoty abonamentu"
        | _ -> Success {id = z.id; zamowienie = z.zamowienie; odleglosc = System.Math.Round(r.NextDouble()*10.0, 2)}

// weryfikacja dostępności usługi na podstawie lokalizacji i odległości oraz określenie rzeczywiście osiągalnych prędkości usługi
let weryfikacjaWarunkowTechnicznych (z: ZamowieniePrzetworzone) = 
    match z.zamowienie.lokalizacja with
    | l when System.String.Equals(l, "Gdynia") || System.String.Equals(l, "Gdańsk")-> 
        match z.odleglosc with
        | o when o > 5 && z.zamowienie.lokalizacja="Gdańsk"-> Error $"Zbyt duża odległość od infrastruktury w lokalizacji {z.zamowienie.lokalizacja}: {o}km"
        | _ -> Success {id = z.id; zamowienie = z.zamowienie; predkoscRzeczywista = z.zamowienie.predkoscOczekiwana}
    | _ -> Error $"Brak dostępności usług w tej lokalizacji: {z.zamowienie.lokalizacja}"

// utworzenie oferty dla klienta
let utworzenieOferty (z:DzialaniePotwierdzone) =
    let parametryUslugi (z: DzialaniePotwierdzone) = 
        let predkosc = z.predkoscRzeczywista;
        let maksymalnaCena = z.zamowienie.maksymalnaCena;
        match predkosc with
        | 10 when maksymalnaCena >= 25 -> (10, 25);
        | 20 when maksymalnaCena >= 40 -> (20, 40);
        | 40 when maksymalnaCena >= 60 -> (40, 60);
        | 60 when maksymalnaCena >= 80 -> (60, 80);
        | 100 when maksymalnaCena >= 115 -> (100, 115);
        | 200 when maksymalnaCena >= 150 -> (200, 150);
        | 300 when maksymalnaCena >= 200 -> (300, 200);
        | x when maksymalnaCena = int (float x*1.5) -> (int (float x*1.5), x) ;
        | _ when z.zamowienie.predkoscMinimalna < int(float maksymalnaCena*(2.0/3.0)) -> (int(float maksymalnaCena*(2.0/3.0)), maksymalnaCena)
        | _ -> (-1, -1)
    
    if(z.predkoscRzeczywista < z.zamowienie.predkoscMinimalna) then
        Error "Rzeczywista prędkość mniejsza niż minimalna akceptowalna, nie utworzono oferty"
    else
        let parametry = parametryUslugi z
        let predkosc = fst parametry
        let abonament = snd parametry
        if parametry = (-1, -1) then
            Error "Nie można dostarczyć usługi mieszczącej się w podanych parametrach"
        else
        Success {id = z.id; predkosc = predkosc; abonament = abonament; zamowienie = z.zamowienie}
    
let podpisanieUmowy (z: Oferta) =
    Success {id = z.id; predkosc = z.predkosc; abonament = z.abonament}

let bind f = 
    fun res ->
        match res with 
        | Success z -> f z
        | Error e -> Error e

//Zad 1a
let validateRequestA = 
    przyjecieZamowienia
    >> bind walidacjaZamowienia
    >> bind weryfikacjaWarunkowTechnicznych
    >> bind utworzenieOferty
    >> bind podpisanieUmowy

let zad1a() = 
    //zawsze sukces
    let zamowienie1 = {lokalizacja = "Gdynia"; predkoscOczekiwana = 300; predkoscMinimalna=80; maksymalnaCena = 650}
    //zawsze błąd
    let zamowienie2 = {lokalizacja = "Kartuzy"; predkoscOczekiwana = 100; predkoscMinimalna=80; maksymalnaCena = 15}
    printfn $"\nZadanie 1a:\n\n{validateRequestA zamowienie1}"
    printfn $"{validateRequestA zamowienie2}\n\n"


let (>>=) twoTrackInput f =
    bind f twoTrackInput

let validateRequestB req=
    req
    >>= przyjecieZamowienia
    >>= walidacjaZamowienia
    >>= weryfikacjaWarunkowTechnicznych
    >>= utworzenieOferty
    >>= podpisanieUmowy

let zad1b() =
    //zawsze sukces
    let zamowienie3 = {lokalizacja = "Gdynia"; predkoscOczekiwana = 300; predkoscMinimalna=80; maksymalnaCena = 650}
    //zawsze błąd
    let zamowienie4 = {lokalizacja = "Kartuzy"; predkoscOczekiwana = 100; predkoscMinimalna=80; maksymalnaCena = 15}
    printfn $"\nZadanie 1b:\n\n{validateRequestB (Success zamowienie3)}"
    printfn $"{validateRequestB (Success zamowienie4)}\n\n"

let zad1c() =
    None

zad1a()
zad1b()
//zad1c()