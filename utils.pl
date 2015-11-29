
available_timeslots(T):-
	findall(room(Room,AvailableSlots),
			bagof(slot(Day,Start,Stop),
			      availability(Room,Day,Start,Stop),
			      AvailableSlots),
			T).

% removeOne(?List, ?E, ?NewList) <- remove first occurence of E
%									in List, resulting in NewList
removeOne([E|R],E,R).
removeOne([H|T],E,[H|Z]):-
	H \= E,
	removeOne(T,E,Z).

takes_exam(Student,Exam):-
	has_exam(Course,Exam),
	follows(Student,Course).

required_capacity(Exam,RequiredCapacity):-
	findall(S,takes_exam(S,Exam),Students),
	length(Students,RequiredCapacity).




is_valid(schedule(EventList)):-
	findall(E,exam(E,_),Exams),
	available_timeslots(FreeSlots),
	is_valid(EventList,Exams,FreeSlots,[]).


is_valid([],[],_,_).
is_valid([event(Exam,Room,Day,Hour)|Events],Exams,Slots,Reservations):-
	remove_one(Exams,Exam,Remaining),
	!, %red cut <- consider only one exam each time
	exam_students(Exam, Students),
	exam_lecturer(Exam, Lecturer),
	length(Students, ReqCapacity),
	free_slot(Slots,slot(Room,Day,Hour,End),NewSlots),
	


	room_suitable(Exam,Room),
	room_available(Exam,Room,Day,Hour,End),
	NewEntry = exam(Exam,Room,Day,Hour,End),
	not(conflict(NewEntry,Reservations)),
	is_valid(Events,Remaining,[NewEntry|Reservations]).



