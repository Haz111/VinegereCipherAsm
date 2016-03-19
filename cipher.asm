
dane1 segment 
    
args	db 128 dup (?) ;200 bajtow na argumenty, maja etykiete "args"
dlugoscArg db 128 dup (0) ;dlugosc poszczegolnych argumentow - w komorce +0 nic, w komorce +1 pierwszy arg, itd
offsetArg db 128 dup (?) ; miejsce w ktorym znajduja sie poszczegolne argumenty
dlugosc dw 0 ; dlugosc argumentow   
iloscArg dw 0 ; ilosc argumentow

arg1 db 50 dup (?)
arg2 db 50 dup (?)
arg3 db 50 dup (?)
arg4 db 50 dup (?)
arg5 db 50 dup (?) 

deszyfrowanie db ? ; ustawione na 1 jezeli tryb deszyfrowania

input db 50 dup (?)
output db 50 dup (?)
kod db 50 dup (?)                                     
dlugoscKodu dw ? ; dlugosc klucza

inputHandler dw ?
outputHandler dw ?

buforOdczytu db 1024 dup (?)
buforZapisu db 1024 dup (?)

error0  db "Nie podales argumetnow! $"
error1  db "Podales za malo zmiennych! Potrzebuje przynajmniej trzech.$"  
error10  db "Nie udalo sie otworzyc pliku. $" 
error11 db "Nie udalo sie zamknac pliku. $"
error12 db "Blad przy odczycie z pliku do bufora $"
error14 db "Blad przy zapisie bufora do pliku $"
error16 db "Blad przy tworzenie/trunkate pliku $"
errorProgramKonczyDzialanie db "Program konczy swoje dzialanie z powodu podanego bledu. $"

errorNieIstnieje db "Blad nie istnieje :( $"

komunikat0 db "Wlaczono tryb szyfrowania $"
komunikat1 db "Podales 4 lub wiecej argumentow, ale to nie jest tryb deszyfrowania $"
komunikat2 db "Wlaczono tryb deszyfrowania $"
komunikat10 db "Operacja otwarcia pliku przebiegla poprawnie. $"
komunikat11 db "Operacja zamkniecia pliku przebiegla poprawnie. $"
komunikat12 db "Poprawnie wczytano porcje 1024 bajtow do bufora. $"
komunikat13 db "Poprawnie wczytano do bufora mniej niz 1024 bajty. $"
komunikat14 db "Poprawnie zapisano caly otrzymany tekst do pliku. $"
komunikat15 db "Poprawnie zapisano mniejsza niz planowano porcje do pliku. O.o why? $"
komunikat16 db "Poprawnie utworzono/truncatowano plik. $"
komunikat20 db "Program przeprowadza szyfrowanie porcji danych zawartych w buforze. $"
komunikat21 db "Program przeprowadza deszyfrowanie porcji danych zawartych w buforze. $"
komunikat22 db "Dane z bufora zostaly przetworzone i zapisane w pliku wyjsciowym. $"
komunikat99 db "Program konczy swoje dzialanie. $"

komunikatNieIstnieje db "Komunikat nie istnieje :( $"


dane1 ends




code1 segment

start1:
	;inicjowanie stosu
	mov	sp, offset wstosu ;sp - wskaznik stosu (stack pointer)
	mov	ax, seg wstosu ;przenosi adres 
	mov	ss, ax   ; ss - stack segment - poczatek segmentu przeznaczonego na stos
	    
	mov bx,0     ;zeruje bx, przed uzyciem bl
	mov bl, byte ptr es:[080h] ; w bl (bx) zapisujemy ilosc znakow argumentow (domyslnie pod adresem 80h) 
    
    cmp bx, 1h   
    ja saArgumenty    ; A > B --> OK
  
;zadziala jezeli nie ma argumentow    
    mov ax, seg args  ;wrzucam do ds segment danych (po to zeby poprawnie wyswietlilo blad) - inaczej nie dziala i wyswietla kod z dziwnego miejsca
    mov ds,ax
	
	mov ax,0 ;wywolanie obslugi bledow
	call obslugaBledow

saArgumenty:    
    
    mov ax, seg args
    mov ds,ax
    
    
    ;
    mov	ax, seg args ;przenosi adres segmentu argumentow do ax 
	mov	ds, ax       ;a potem z ax do ds
	mov	si, offset args ;   ds:[si]  - pelny logiczny adres argumentow zapisanych w pamieci
	;
	mov	di,0  ; zerowanie rejestru di (bedzie offsetem dla pierwotnych argumentow)
	
	mov cx, 0
	mov cx,bx ;ilosc powtorzen 
	dec cx    ; na koncu CX jest zawarty dodatkowy znak "0D" ktory psuje. Pomijamy go
	mov word ptr ds:[dlugosc], bx;                  
	
	mov bx, 0 ; czyszczenie bx
	mov ax, 0 ; czyszczenie akumulatora
	
	call wczytajArg  ; procedura 1
	                    
	call poprawnosc  ; sprawdza poprawnosc klucza i od razu wczytuje go do odpowiednich zmiennych
	
	call dzialanie
	
	mov ax, 99d
	call obslugaKomunikatow
	
koniecProgramu:
	mov	ah,4ch  ; zakoncz program i wroc do systemu
	int	021h              
	
	
	
;................................................
;................................................

;             P R O C E D U R Y    

;................................................
;................................................

; INSTANCJA SZYFROWANIA
dzialanie:
push ax
push bx
push cx
push dx
push si
push di
pushf

mov ax, offset input 
xor bx, bx
mov bl, 0d
call otworzPlik ;pobiera w AX offset nazwy pliku do otwarcia, zwraca Handler
mov  word ptr ds:[inputHandler], ax  ; zapisuje Handler inputa w zmiennej inputHandler

mov ax, offset output 
call tworzTruncatePlik ; tworzy/truncatuje plik. Pobiera w AX offset nazwy pliku, zwraca handler
mov  word ptr ds:[outputHandler], ax  ; zapisuje Handler outputa w zmiennej outputHandler

dzialanie_przetwarzanie:
	mov bx, ds:[inputHandler]
	mov ax, 1024d
	call wczytajDoBufora      	;przyjmuje podany w BX handler, zwraca ilosc odczytanych bajtow w AX
	push ax
	   
		cmp byte ptr ds:[deszyfrowanie], 1d			;robie to tutaj, poniewaz co prawda bedzie sie wykonywalo przy kazdym nowym buforze, ale w wiekszosci przypadkow buforow nie bedzie az tyle zeby to drastycznie wplynelo na czas wykonania
		je dzialanie_deszyfrowanie
			call szyfrujBufor
			jmp dzialanie_poPrzetworzeniuBufora
		
		dzialanie_deszyfrowanie:
			call deszyfrujBufor
		
	dzialanie_poPrzetworzeniuBufora:
	pop ax
	push ax
	mov bx, ds:[outputHandler]
	call zapiszBuforDoPliku
	pop ax
	
cmp ax, 1024d	
je dzialanie_przetwarzanie

	mov ax, 22d
	call obslugaKomunikatow

;zamykanie plikow	
mov ax, word ptr ds:[inputHandler]
call zamknijPlik ;pobiera w AX Handler do zamkniecia

mov ax, word ptr ds:[outputHandler] 
call zamknijPlik 
	
popf
pop di
pop si
pop dx
pop cx
pop bx
pop ax

ret

;................................................
;................................................

; INSTANCJA 4: SZYFR VIGENERE

; szyfr Vigenere'a dla tabeli o labelach 0-255 na 0-255 mozemy zapisac wzorem W = (X + K) mod 256, gdzie W=Wynik, X=zmienna, K=Kod. Ponizej jest interpretacja tego wzoru

szyfrujBufor: ;przyjmuje w AX ilosc odczytanych znakow, ale nic nie zwraca, dlatego pushuje i popuje AX
push ax
push bx
push cx
push dx
push si
push di
pushf
	
	mov cx, ax ;ilosc powtorzen
	xor si, si
	xor di, di
	
	mov ax, 20d
	call obslugaKomunikatow
	
	szyfrujBufor_petlaSzyfrujaca:
		xor bx, bx
		xor dx, dx
		mov bl, byte ptr ds:[buforOdczytu+si]
		mov dl, byte ptr ds:[kod+di]
		xor ax, ax
		add ax, bx
		add ax, dx			
		
		szyfrujBufor_zaDuzeNaZnak:  		; sprawdzenie czy mamy znak czy wielokrotnosc
			cmp ax, 255d
			jle szyfrujBufor_mamyPoprawnyZnak			; A <= B
			
			sub ax, 256d
			jmp szyfrujBufor_zaDuzeNaZnak
			
		szyfrujBufor_mamyPoprawnyZnak:
		
		mov byte ptr ds:[buforZapisu+si], al ; bo w BL mamy pozostaly znak zapisany w jednym bajcie
		
		inc si
		inc di
		
		cmp di, word ptr ds:[dlugoscKodu]
		jb szyfrujBufor_nieZaczynajKoduOdNowa		; A < B
			xor di, di		;jedziemy od nowa z tym kodem ;)
		szyfrujBufor_nieZaczynajKoduOdNowa:
		
	loop szyfrujBufor_petlaSzyfrujaca
	
popf
pop di
pop si
pop dx
pop cx
pop bx
pop ax

ret

; ////////// DESZYFROWANIE BUFORA \\\\\\\\\\\\\\\

; Funkcje deszyfrowania mozemy zapisac jako 

deszyfrujBufor: ;przyjmuje w AX ilosc odczytanych znakow, ale nic nie zwraca, dlatego pushuje i popuje AX
push ax
push bx
push cx
push dx
push si
push di
pushf
	
	mov cx, ax ;ilosc powtorzen
	xor si, si
	xor di, di
	
	mov ax, 21d
	call obslugaKomunikatow
	
	deszyfrujBufor_petlaSzyfrujaca:
		xor bx, bx
		xor dx, dx
		mov bl, byte ptr ds:[buforOdczytu+si]
		mov dl, byte ptr ds:[kod+di]
		xor ax, ax
		cmp bx, dx
		jae	deszyfrujBufor_normalnySub			; A >= B
		;jezeli kod jest wiekszy niz wartosc
		sub dx, bx ;w dx zostaje nam roznica (na plus) kodu
		mov ax, 256d
		sub ax, dx ; w ax mamy piekna, odszyfrowana wartosc
		
		deszyfrujBufor_normalnySub:
		sub bx, dx
		mov ax, bx
				
		mov byte ptr ds:[buforZapisu+si], al ; bo w BL mamy pozostaly znak zapisany w jednym bajcie
		
		inc si
		inc di
		
		cmp di, word ptr ds:[dlugoscKodu]
		jb deszyfrujBufor_nieZaczynajKoduOdNowa		; A < B
			xor di, di		;jedziemy od nowa z tym kodem ;)
		deszyfrujBufor_nieZaczynajKoduOdNowa:
		
	loop deszyfrujBufor_petlaSzyfrujaca
	
popf
pop di
pop si
pop dx
pop cx
pop bx
pop ax

ret

;................................................
;................................................

; INSTANCJA 3: OPERACJE NA PLIKACH

;//////////   OTWARCIE   PLIKU   \\\\\\\\\\\\\\\\\

otworzPlik: ; pobiera w AX offset pliku do otwarcia, bedzie zwracal Handler, w BL tryb odczytu
push cx
push dx
push si
push di
pushf

	mov dx, ax ; offset zmiennej z nazwa pliku do otwarcia; ds:dx - nazwa pliku
	mov al, bl    ; tryb do odczytu (0 - odczyt, 1 - zapis, 2 - odczyt + zapis)
	mov ah, 3dh  ; funkcja "3Dh" przerwania 21h otwiera plik 
	int 21h   ; przerwanie 21h. Otwieramy plik
	
	JC otworzPlik_inputBlad ; jezeli flaga cf ustawiona (cf = 1) to wystapil blad
	
	push ax
		mov ax, 10d
		call obslugaKomunikatow
	pop ax
	
jmp otworzPlik_koniec
	
	
otworzPlik_inputBlad:
	mov ax, 10d
	call obslugaBledow ;papatki, koniec programu
	
otworzPlik_koniec:	
popf
pop di
pop si
pop dx
pop cx

ret

; //////////// UTWORZ / TRUNCATUJ PLIK \\\\\\\\\\\

tworzTruncatePlik: ; pobiera w AX offset pliku do utworzenia/truncatowania, bedzie zwracal Handler
push bx
push cx
push dx
push si
push di
pushf

	mov dx, ax ; offset zmiennej z nazwa pliku do utworzenia; ds:dx - nazwa pliku
	mov cl, 0d    ; atrybuty pliku - 0 - bez atrybutow
	mov ah, 3Ch  ; funkcja "3Dh" przerwania 21h tworzy/truncatuje plik
	int 21h   ; przerwanie 21h. Otwieramy plik
	
	JC tworzTruncatePlik_tworzenieBlad ; jezeli flaga cf ustawiona (cf = 1) to wystapil blad
	
	push ax
		mov ax, 16d
		call obslugaKomunikatow
	pop ax
	
jmp tworzTruncatePlik_koniec
	
	
tworzTruncatePlik_tworzenieBlad:
	mov ax, 16d
	call obslugaBledow ;papatki, koniec programu
	
tworzTruncatePlik_koniec:	
popf
pop di
pop si
pop dx
pop cx
pop bx

ret

;//////////   ZAMKNIECIE   PLIKU   \\\\\\\\\\\\\\\\\

zamknijPlik: ; pobiera w AX Handler pliku
push bx
push cx
push dx
push si
push di
pushf

	mov bx, ax ; przenosi handler do bx - ta jak ma byc dla funkcji nr 3Eh przerwania 21h
	mov ah, 3Eh  ; funkcja "3Eh" przerwania 21h zamyka plik 
	int 21h   ; przerwanie 21h. Otwieramy plik
	
	JC zamknijPlik_inputBlad ; jezeli flaga cf ustawiona (cf = 1) to wystapil blad
	
	push ax
		mov ax, 11d ; wszystko OK!
		call obslugaKomunikatow
	pop ax
	
jmp zamknijPlik_koniec
	
	
zamknijPlik_inputBlad:
	mov ax, 11d
	call obslugaBledow ;papatki, koniec programu	
	
zamknijPlik_koniec:	
popf
pop di
pop si
pop dx
pop cx
pop bx

ret


;////////////    CZYTANIE Z PLIKU    \\\\\\\\\\\\\\\\\\\\\
 
wczytajDoBufora: ; argumenty: AX - ile przeczytac, BX - handler
push bx
push cx
push dx
push si
push di
pushf

	mov cx, ax					; tyle bedziemy czytac na raz do bufora
	; handler zostal podany jako argument w BX
	mov ah, 3Fh					; funkcja 3Fh przerwania 21h - czytanie z pliku
	mov dx, offset buforOdczytu	; zapisujemy bajty do bufora odczytu
	int 21h						; przerwanie 21h

	jc wczytajDoBufora_bladOdczytuDoBufora ; jezeli cf=1 to byl blad

	cmp ax, cx ; sprawdza czy wczytano full 1024 bajty czy juz mniej
	je wczytajDoBufora_wczytanoFull
	jmp wczytajDoBufora_wczytanoNieFull

wczytajDoBufora_bladOdczytuDoBufora:
	mov ax, 12d
	call obslugaBledow 
;program konczy swoja prace po bledzie

wczytajDoBufora_wczytanoFull:
	push ax
	mov ax, 12d
	call obslugaKomunikatow
	pop ax
jmp wczytajDoBufora_koniec

wczytajDoBufora_wczytanoNieFull:
	push ax
	mov ax, 13d
	call obslugaKomunikatow
	pop ax
jmp wczytajDoBufora_koniec
	
wczytajDoBufora_koniec:

popf
pop di
pop si
pop dx
pop cx
pop bx

ret

;////////////    ZAPIS DO PLIKU    \\\\\\\\\\\\\\\\\\\\\ 

zapiszBuforDoPliku: ; argumenty: AX - ile zapisac
push bx
push cx
push dx
push si
push di
pushf

	mov cx, ax					; tyle bajtow bedziemy pisac do pliku
	; handler zostal podany jako argument w BX
	mov ah, 40h					; funkcja 40h przerwania 21h - pisanie z pliku
	mov dx, offset buforZapisu ; zapisujemy bajty z bufora zapisu do pliku
	int 21h						; przerwanie 21h

	jc zapiszBuforDoPliku_bladZapisuDoBufora ; jezeli cf=1 to byl blad

	cmp ax, cx ; sprawdza czy wczytano full 1024 bajty czy juz mniej
	je zapiszBuforDoPliku_zapisanoFull
	jmp zapiszBuforDoPliku_zapisanoNieFull

zapiszBuforDoPliku_bladZapisuDoBufora:
	mov ax, 14d
	call obslugaBledow 
;program konczy swoja prace po bledzie

zapiszBuforDoPliku_zapisanoFull:
	push ax
	mov ax, 14d
	call obslugaKomunikatow
	pop ax
jmp zapiszBuforDoPliku_koniec

zapiszBuforDoPliku_zapisanoNieFull:
	push ax
	mov ax, 15d
	call obslugaKomunikatow
	pop ax
jmp zapiszBuforDoPliku_koniec
	
zapiszBuforDoPliku_koniec:

popf
pop di
pop si
pop dx
pop cx
pop bx

ret

;///////     PRZESUWANIE WSKAZNIKA PLIKU \\\\\\\\\\\\\\\ 

przesunWskaznikPliku:  ; AX - o ile przesunac, BX - handler
push ax
push bx
push cx
push dx
push si
push di
pushf
	
	mov cx, 0d   	; cx:dx  - o tyle funkcja 42h przerwania 21h przesuwa wskaznik
	mov dx, ax  	; DX = AX czyli o tyle ma sie przesunac wskaznik
	mov ah, 42h		; funkcja 42h przerwania 21h - przestawienie wskaznika pliku
	mov al, 1d 		; przesuwamy wzgledem biezacej pozycji wskaznika (0 - poczatek, 1 - biezaca pozycja, 2 - koniec pliku)
	; BX juz zawiera handler podany jako argument
	int 21h			; przerwanie 21h
	
przesunWskaznikPliku_koniec:
popf
pop di
pop si
pop dx
pop cx
pop bx
pop ax

ret
 
 
;................................................
;................................................
 
; INSTANCJA 2: POPRAWNOSC I INTERPRETACJA

; 1 /////////////  POPRAWNOSC  \\\\\\\\\\\\

poprawnosc:
push ax
push bx
push cx
push dx
push si
push di
pushf

	cmp word ptr ds:[iloscArg], 2d ; A <= B
	jbe poprawnosc_zaMaloArg

	cmp word ptr ds:[iloscArg], 3d	; A = B
	je poprawnosc_ustawTrybSzyfrowania

; A >= 4
	jmp poprawnosc_sprawdzCzyTrybDeszyfrowania

poprawnosc_zaMaloArg:
	mov ax, 1d
	call obslugaBledow  ; nastapi zakonczenie programu

poprawnosc_ustawTrybSzyfrowania:
	mov byte ptr ds:[deszyfrowanie], 0d
	mov ax, 0d
	call obslugaKomunikatow ;wlaczono tryb szyfrowania
	;przepisywanie argumentow
	mov ax, 1d
	call ustawienieInput
	mov ax, 2d
	call ustawienieOutput
	mov ax, 3d
	call ustawienieKod
jmp poprawnosc_koniec

poprawnosc_sprawdzCzyTrybDeszyfrowania:
	mov ax, 1d
	call dlugoscArgumentu
	cmp ax, 2d ;jezeli rowne to znaczy, ze moze tam byc "-d" wiec sprawdzamy dalej
	jne  poprawnosc_toNieJestDeszyfrowanie ; jezeli nie rowne to nie bedzie tam "-d" wiec mamy tryb szyfrowania, wyswietli monit
	
	cmp byte ptr ds:[arg1], '-'
	jne poprawnosc_ustawTrybSzyfrowania
	cmp byte ptr ds:[arg1+1], 'd'
	jne poprawnosc_ustawTrybSzyfrowania
jmp poprawnosc_ustawTrybDeszyfrowania ; ustawiam tryb deszyfrowania
	
poprawnosc_toNieJestDeszyfrowanie: ;wyswietla monit i jedzie dalej
	mov ax, 1d   ; monit "Podales 4 lub wiecej argumentow, ale to nie jest tryb deszyfrowania"
	call obslugaKomunikatow
jmp poprawnosc_ustawTrybSzyfrowania

poprawnosc_ustawTrybDeszyfrowania:
	mov byte ptr ds:[deszyfrowanie], 1d
	mov ax, 2d  ; komunikat 2 - tryb deszyfrowania
	call obslugaKomunikatow ;wlaczono tryb deszyfrowania
	;przepisywanie argumentow
	mov ax, 2d
	call ustawienieInput
	mov ax, 3d
	call ustawienieOutput
	mov ax, 4d
	call ustawienieKod
jmp poprawnosc_koniec

poprawnosc_koniec:
popf
pop di
pop si
pop dx
pop cx
pop bx
pop ax
ret ;powroct do glownego programu


ustawienieInput: ; wczytuje przekazane do funkcji ax
	push bx
	push cx
	push dx
	push si
	push di
		call ustawienieDoKopiowania ; AX = 0, BX - nr arg, CX - dlugosc arg, SI - offset arg, DI =  0
		ustawienieInput_kopiujArgument: 
            mov al, byte ptr ds:[args+si]
            mov byte ptr ds:[input+di], al
            inc si
            inc di    
        loop ustawienieInput_kopiujArgument
        ;mov byte ptr ds:[input+di], 24h ;na koniec argumentu dorzuca dolarka
	pop di
	pop si
	pop dx
	pop cx
	pop bx
ret

ustawienieOutput: ; wczytuje przekazane do funkcji ax
	push bx
	push cx
	push dx
	push si
	push di
		call ustawienieDoKopiowania ; AX = 0, BX - nr arg, CX - dlugosc arg, SI - offset arg, DI = 0
		ustawienieOutput_kopiujArgument:
            mov al, byte ptr ds:[args+si]
            mov byte ptr ds:[output+di], al
            inc si   
            inc di 
        loop ustawienieOutput_kopiujArgument
        ;mov byte ptr ds:[output+di], 24h ;na koniec argumentu dorzuca dolarka
	pop di
	pop si
	pop dx
	pop cx
	pop bx
ret

ustawienieKod: ; wczytuje przekazane do funkcji ax
	push bx
	push cx
	push dx
	push si
	push di
		call ustawienieDoKopiowania ; AX = 0, BX - nr arg, CX - dlugosc arg, SI - offset arg, DI = 0
		mov word ptr ds:[dlugoscKodu], cx ;zachowuje dlugosc klucza w zmiennej
		ustawienieKod_kopiujArgument:
            mov al, byte ptr ds:[args+si]
            mov byte ptr ds:[kod+di], al
            inc si  
            inc di  
        loop ustawienieKod_kopiujArgument
        mov byte ptr ds:[kod+di], 24h ;na koniec argumentu dorzuca dolarka
	pop di
	pop si
	pop dx
	pop cx
	pop bx
ret

	
ustawienieDoKopiowania: ;bez konwencji wolania bo to jest "ustawiacz" ktory przygotowuje nam rejestry na inna funkcje
	mov bx,ax ;zachowuje nr arg z AX w BX
    call dlugoscArgumentu
    mov cx, ax ;zachowuje dlugosc arg w cx
    mov ax, bx
    call offsetArgumentu  
    mov si, ax
    xor ax, ax 
	xor di, di  ; zerowanie dx
ret




;................................................
;................................................

;             K O M U N I K A T Y   

;................................................
;................................................

; INSTANCJA 0b  SPRAWDZENIE NR KOMUNIKATU I WYSWIETLENIE GO
;przyjmuje nr komunikatu w rejestrze ax
obslugaKomunikatow:
; sprawdzanie komunikatu
cmp ax, 0d
je obslugaKomunikatow_komunikat0

cmp ax, 1d
je obslugaKomunikatow_komunikat1

cmp ax, 2d
je obslugaKomunikatow_komunikat2

cmp ax, 10d
je obslugaKomunikatow_komunikat10 ; poprawnie wczytano plik

cmp ax, 11d
je obslugaKomunikatow_komunikat11

cmp ax, 12d
je obslugaKomunikatow_komunikat12

cmp ax, 13d
je obslugaKomunikatow_komunikat13

cmp ax, 14d
je obslugaKomunikatow_komunikat14

cmp ax, 15d
je obslugaKomunikatow_komunikat15

cmp ax, 16d
je obslugaKomunikatow_komunikat16

cmp ax, 20d
je obslugaKomunikatow_komunikat20	; szyfruje 

cmp ax, 21d
je obslugaKomunikatow_komunikat21	; deszyfruje

cmp ax, 22d
je obslugaKomunikatow_komunikat22

cmp ax, 99d
je obslugaKomunikatow_komunikat99

jmp obslugaKomunikatow_komunikatNieIstnieje

; wyswietlanie monitow
obslugaKomunikatow_komunikat0: ; tryb szyfrowania
    mov dx, offset komunikat0
jmp obslugaKomunikatow_wypiszKomunikat

obslugaKomunikatow_komunikat1: ; tryb szyfrowania
    mov dx, offset komunikat1
jmp obslugaKomunikatow_wypiszKomunikat

obslugaKomunikatow_komunikat2: ; tryb szyfrowania
    mov dx, offset komunikat2
jmp obslugaKomunikatow_wypiszKomunikat

obslugaKomunikatow_komunikat10: ; poprawnie wczytano plik
    mov dx, offset komunikat10
jmp obslugaKomunikatow_wypiszKomunikat

obslugaKomunikatow_komunikat11: ; poprawnie wczytano plik
    mov dx, offset komunikat11
jmp obslugaKomunikatow_wypiszKomunikat

obslugaKomunikatow_komunikat12: ; poprawnie wczytano plik
    mov dx, offset komunikat12
jmp obslugaKomunikatow_wypiszKomunikat

obslugaKomunikatow_komunikat13: ; poprawnie wczytano plik
    mov dx, offset komunikat13
jmp obslugaKomunikatow_wypiszKomunikat

obslugaKomunikatow_komunikat14: ; poprawnie wczytano plik
    mov dx, offset komunikat14
jmp obslugaKomunikatow_wypiszKomunikat

obslugaKomunikatow_komunikat15: ; poprawnie wczytano plik
    mov dx, offset komunikat15
jmp obslugaKomunikatow_wypiszKomunikat

obslugaKomunikatow_komunikat16: ; poprawnie wczytano plik
    mov dx, offset komunikat16
jmp obslugaKomunikatow_wypiszKomunikat

obslugaKomunikatow_komunikat20: ; szyfruje
    mov dx, offset komunikat20
jmp obslugaKomunikatow_wypiszKomunikat

obslugaKomunikatow_komunikat21: ; deszyfruje
    mov dx, offset komunikat21
jmp obslugaKomunikatow_wypiszKomunikat

obslugaKomunikatow_komunikat22: 
    mov dx, offset komunikat22
jmp obslugaKomunikatow_wypiszKomunikat ; zakonczyl przetwarzanie

obslugaKomunikatow_komunikat99: ; poprawnie wczytano plik
    mov dx, offset komunikat99
jmp obslugaKomunikatow_wypiszKomunikat

obslugaKomunikatow_komunikatNieIstnieje: ; tryb szyfrowania
    mov dx, offset komunikatNieIstnieje
jmp obslugaKomunikatow_wypiszKomunikat

obslugaKomunikatow_wypiszKomunikat:
	mov	ah,9  ; wypisz tekst z DS:DX
	int	21h
		push dx
		;enter
        mov dx,13 ;CR
        mov ah,2
        int 021h  
        mov dx,10 ;LF   - Line Feed  
        mov ah,2
        int 021h  ; CRLF - zakonczenie lini w windowsie
		pop dx
;powrot do programu

obslugaKomunikatow_wrocDoProcedury:
ret


;................................................
;................................................

;         O B S L U G A   B L E D O W  

;................................................
;................................................

; INSTANCJA 0  SPRAWDZENIE BLEDU I WYSWIETLENIE KOMUNIKATU
;przyjmuje nr bledu w rejestrze ax
obslugaBledow:
; sprawdzanie bledu
cmp ax, 0d
je obslugaBledow_error0

cmp ax, 1d
je obslugaBledow_error1

cmp ax, 10d
je obslugaBledow_error10

cmp ax, 11d
je obslugaBledow_error11

cmp ax, 12d
je obslugaBledow_error12

cmp ax, 14d
je obslugaBledow_error14

cmp ax, 16d
je obslugaBledow_error16

jmp obslugaBledow_errorNieIstnieje

; wyswietlanie monitow
obslugaBledow_error0: ; brak arg
    mov dx, offset error0
jmp obslugaBledow_koniecProgramu

obslugaBledow_error1: ; za malo arg
    mov dx, offset error1
jmp obslugaBledow_koniecProgramu

obslugaBledow_error10: ; nie udalo sie otworzyc pliku
    mov dx, offset error10
jmp obslugaBledow_koniecProgramu

obslugaBledow_error11: ; blad zamkniecia pliku
    mov dx, offset error11
jmp obslugaBledow_koniecProgramu

obslugaBledow_error12: ; blad odczytu z pliku
    mov dx, offset error12
jmp obslugaBledow_koniecProgramu

obslugaBledow_error14: ; blad zapisu z bufora do pliku pliku
    mov dx, offset error14
jmp obslugaBledow_koniecProgramu

obslugaBledow_error16: ; blad tworz/truncate
    mov dx, offset error16
jmp obslugaBledow_koniecProgramu

obslugaBledow_errorNieIstnieje: ; tryb szyfrowania
    mov dx, offset errorNieIstnieje
jmp obslugaBledow_koniecProgramu

; koniec lub powrot do programu
obslugaBledow_koniecProgramu:
	mov	ah,9  ; wypisz tekst z DS:DX
	int	21h
		;enter
        mov dx,13 ;CR   ; nie obchodzi mnie to ze zmieniam DX, bo i tak koncze program
        mov ah,2
        int 021h  
        mov dx,10 ;LF   - Line Feed  
        mov ah,2
        int 021h  ; CRLF - zakonczenie lini w windowsie
	
	mov dx, offset errorProgramKonczyDzialanie
	mov	ah,9  ; wypisz tekst z DS:DX
	int	21h

jmp koniecProgramu

obslugaBledow_wrocDoProcedury:
	mov	ah,9  ; wypisz tekst z DS:DX
	int	21h
		push dx
		;enter
        mov dx,13 ;CR
        mov ah,2
        int 021h  
        mov dx,10 ;LF   - Line Feed  
        mov ah,2
        int 021h  ; CRLF - zakonczenie lini w windowsie
		pop dx
;powrot do programu

ret

;................................................
;................................................

;                P A R S E R   

;................................................
;................................................

; INSTANCJA 1 WCZYTANIE I KOPIOWANIE ARGUMENTOW
 
; 1 /////////////  WCZYTANIE ARGUMENTOW \\\\\\\\\\\\

wczytajArg:
    mov	al, byte ptr es:[082h + di]
    call przewinBialeZnaki  
    
    mov ds:[offsetArg+di+1], 0d
    
    cmp al, 024h   ;jezeli na poczatku byly spacje to zinkrementuje jeszcze raz di zeby zaczynac pierwszy argument od zera
    jne wczytArg
    
    inc di
    	
	    wczytArg:   ;glowna petla wczytywania argomentow 
	    ;  !!! NIE PUSHUJE CX PONIEWAZ ZMIENIAM GO JESZCZE W INNYM MIEJSCU!!!
	        mov	al, byte ptr es:[082h + di]
	        call przewinBialeZnaki                

	        mov	byte ptr ds:[args+si],al
	         
	            cmp al, 024h
	            jne nieNowy       ;jezeli al rozne od dolara skacz do nieNowy
	            inc ds:[iloscArg] ;jezeli rowne inkrementuj ilosc Argumentow
					push ax
					push bx
					push si
					push di
					    mov ax, ds:[iloscArg] ; zapisuje ilosc argumentow w ax 
					    mov di, ax
					    mov bx, si ; zapisuje w bx miejsce w ktorym jest przerwa
					    inc bl     ; a teraz miejsce w ktorym zaczyna sie nastepny arg
					    mov ds:[offsetArg+di+1], bl           ;+ax+2  
					    
					    sub bl, ds:[offsetArg+di]
					    dec bl   ; coby dlugosc byla bez dolara
					    
					    mov ds:[dlugoscArg+di], bl      
					pop di
					pop si
					pop bx
					pop ax
					
	            nieNowy:
	        
	        inc	si  ; si = si+1 
	        inc	di  ; di = di+1 - inkrementacja rejestru pierwotnych argumentow
	        
	    loop wczytArg 
	    
	;na koncu musi byc dolar  
	cmp byte ptr ds:[si-1], 024h ; sprawdzam czy na koncu jest dolar
	je bialyKoniec
    ;jezeli nie ma to wstawiam   
	mov al, 24h    ; wstawiam dolara do al
	mov    byte ptr ds:[si],al    ; a pozniej za ostatni argument
	inc ds:[iloscArg] ;inkrementuje ostatni raz dlugosc argumentow
bialyKoniec:
	    
	mov ax, si
	mov ds:dlugosc,ax  ;zapisanie dlugosci wszystkich argumentow do 
	
						mov ax, ds:[iloscArg] ; zapisuje ilosc argumentow w ax 
					    mov di, ax
					    mov bx, si ; zapisuje w bx miejsce w ktorym jest przerwa
					    inc bl     ; a teraz miejsce w ktorym zaczyna sie nastepny arg 
					    
					    sub bl, ds:[offsetArg+di]
					    dec bl   ; coby dlugosc byla bez dolara
					    
					    mov ds:[dlugoscArg+di], bl    
					    
    call pierwsze5arg
	
ret ;koniec
                                  
                                  
; 2 /// PRZEWIN BIALE ZNAKI \\\\\\\   
; sprawdza w petli while czy kolejny znak jest bialy, jezeli tak to go pomijam. Zwracam w AL znak dolara,
; jezeli przesuniecie zostalo wykonane. Jezeli nie - zwracam oryginalny znak  
przewinBialeZnaki:
push bx    ; zachowuje bx na stosie, poniewaz bedzie modyfikowany w ciele procedury
push dx

mov bl, al 

call sprawdzCzyBialy

cmp ax, 1d ;jezeli procedura zwrocila 1 to znak jest bialy, jezeli 0 to nie bialy 
    mov	al, byte ptr es:[082h + di]	 ; przeniesie do al ostatni znak i zworoci go z procedury
jne przewinBialeZnaki_normalnyZnak ; jezeli znak nie jest bialy to nie przewija
    
przewinBialeZnaki_bialyZnak:
    inc	di  ; di = di+1 - inkrementacja rejestru pierwotnych argumentow 
    dec cx
    mov	bl, byte ptr es:[082h + di] ;przechodze do nastepnego znaku
    
    call sprawdzCzyBialy
    
    cmp ax, 1d
    je przewinBialeZnaki_bialyZnak
        
    dec di ;dekrementuje di zeby nie dublowac inkrementacji
    inc cx
        mov	al, 24h ; zwroci z procedury dolara

przewinBialeZnaki_normalnyZnak: 
      
pop dx
pop bx    
ret 


; 2a ////// SPRAWDZ CZY ZNAK JEST BIALY \\\\\\\\\\\\\\\\
sprawdzCzyBialy:    ;zwraca wynik w AX
push bx    ; zachowuje bx na stosie, poniewaz bedzie modyfikowany w ciele procedury
push cx
push dx

    ;jmp sprawdzBialeZnaki
    cmp bl, 032d ; porownuje skopiowany bajt do spacji
	je sprawdzCzyBialy_bialy
	cmp bl, 009d ; porownuje skopiowany bajt do taba
    je sprawdzCzyBialy_bialy
    
	;mamy normlany znak
    mov ax,0d
	jmp sprawdzCzyBialy_powrot
    
    sprawdzCzyBialy_bialy:
    mov ax,1d
	jmp sprawdzCzyBialy_powrot
    
sprawdzCzyBialy_powrot:
pop dx
pop cx
pop bx 
ret  

; ////// DLUGOSC ARG O DANYM NUMERZE \\\\\\\\\\\\\
;pobiera w ax numer argumentu i zwraca jego dlugosc w ax
dlugoscArgumentu:
push bx
push si
    mov si, ax
    xor ax, ax
    mov al, ds:[dlugoscArg+si]

pop si
pop bx 
ret  

; ////// OFFSET ARG O DANYM NUMERZE \\\\\\\\\\\\\
;pobiera w ax numer argumentu i zwraca jego dlugosc w ax
offsetArgumentu:
push bx
push si
    mov si, ax
    xor ax, ax
    mov al, ds:[offsetArg+si]

pop si
pop bx 
ret


; 3 //////////// PIERWSZE 5 ARG \\\\\\\\\\\\\\\\\\\\\\
pierwsze5arg:
push ax
push bx
push cx
push dx
push di
push si
    
    mov si, 1d
    xor di, di
      
    cmp ds:[iloscArg], 5
    ja pierwsze5arg_mam5arg ;skacze jezeli ilosc arg wynosi 5 lub wiecej
    
    mov cx, ds:[iloscArg]
    jmp pierwsze5arg_petlaKopiujacaArgumenty
    
    pierwsze5arg_mam5arg:
    mov cx, 5d
    
    pierwsze5arg_petlaKopiujacaArgumenty:
        mov ax, si 
        call dlugoscArgumentu 
            push cx 
            push di
                mov cx, ax
                mov ax, si
                call offsetArgumentu  
                push si
                mov si, ax
                xor ax, ax 
                pierwsze5arg_kopiujArgument:
                    mov al, ds:[args+si]
                    mov ds:[arg1+di], al
                    inc si
                    inc di     
                loop pierwsze5arg_kopiujArgument
                mov ds:[arg1+di], 24h ;na koniec argumentu dorzuca dolarka
                pop si
            pop di
            pop cx
        add di, 30d
        inc si        
    loop pierwsze5arg_petlaKopiujacaArgumenty          

pop si
pop di
pop dx
pop cx
pop bx
pop ax
ret



code1 ends



stos1 segment stack
		dw 200 dup(?)
wstosu	dw ?
stos1 ends


end start1