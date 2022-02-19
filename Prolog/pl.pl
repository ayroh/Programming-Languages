%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PART 1
% knowledge base

flight(canakkale,erzincan,6).
flight(erzincan,canakkale,6).

flight(erzincan,antalya,3).
flight(antalya,erzincan,3).

flight(diyarbakir,antalya,4).
flight(antalya,diyarbakir,4).

flight(izmir,antalya,2).
flight(antalya,izmir,2).

flight(izmir,ankara,6).
flight(ankara,izmir,6).

flight(ankara,diyarbakir,8).
flight(diyarbakir,ankara,8).

flight(istanbul,izmir,2).
flight(izmir,istanbul,2).

flight(istanbul,ankara,1).
flight(ankara,istanbul,1).

flight(istanbul,rize,4).
flight(rize,istanbul,4).

flight(rize,ankara,5).
flight(ankara,rize,5).

flight(ankara,van,4).
flight(van,ankara,4).

flight(van,gaziantep,3).
flight(gaziantep,van,3).

% rules
% this allows us to find all connected nods by route(ankara,Y,C). or check spesific flight by route(ankara,istanbul,C). or route(ankara,istanbul,1).
route(X,Y,C) :- flight(X,Y,C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PART 2

% knowledge base

:- dynamic room/5. % for dynamicly creating datas with assertz
:- dynamic course/8.
:- dynamic student/3.




%room id, capacity, hours, equipment, ok for handicapped
room(c1,40,[8,9,10,11,12,13,14,15,16],projector,yes).
room(c2,40,[8,9,10,11,12,13,14,15,16],smartboard,yes).
room(c3,40,[8,9,10,11,12,13,14,15,16],nothing,yes).
room(c4,40,[8,9,10,11,12,13,14,15,16],nothing,no).

%class id, instructor, capacity, hours, class, attendents, equipment
course(cse341,1,10,[8,9,10],c1,[2,3,9,10],projector).
course(cse343,2,6,[9,10,11],c2,[6,7,8,9,10],smartboard).
course(cse102,3,5,[12,13,14,15],c1,[2,3,4,5,6,7,8],nothing).										
course(cse101,4,10,[13,14,15],c2,[1,2,3,4,5],nothing).						 

%room id, class id, hours
occupancy(c1,cse341,[8,9,10]).
occupancy(c2,cse343,[12,13,14,15]).
occupancy(c3,cse102,[9,10,11]).
occupancy(c4,cse101,[13,14,15]).

%instructor id, classes, wanted equipment
instructor(1,[cse341],projector).
instructor(2,[cse343],smartboard).
instructor(3,[cse102],nothing).
instructor(4,[cse101],nothing).

%student id, classes, is handicapped
student(1,[cse101],no).
student(2,[cse101,cse341],no).
student(3,[cse101,cse341],no).
student(4,[cse101,cse102],no).
student(5,[cse101,cse102],no).
student(6,[cse343,cse102],no).
student(7,[cse102,cse343],no).
student(8,[cse102,cse343],no).
student(9,[cse341,cse343],no).
student(10,[cse341,cse343],yes).


% rules
%add dynamicly according to inputs
addCourse(Id,Instructor,Capacity,Hours,RoomId,StudentIds,Equipment,H) :- assertz(course(Id,Instructor,Capacity,Hours,RoomId,StudentIds,Equipment,H)).
addStudent(Id,Courses,H) :- assertz(student(Id,Courses,H)).
addRoom(Id,Capacity,Hours,Equipment,H) :- assertz(room(Id,Capacity,Hours,Equipment,H)).

% classToStudent(1,Course). or classToStudent(1,cse341).
classToStudent(Student,Course) :- student(Student,ListCourses,H), % find student
                                 course(Course,_,Capacity,_,Room,List,_), % find course
                  				 not(checkOverlap(Course,ListCourses)), % checks if students courses conflict with wanted course
                                 isCapacitySmaller(List, Capacity), % checks that courses capacity
                                 (H = 'yes' -> room(Room,_,_,_,yes); room(Room,_,_,_,no)) . % checks room according to handicapped or not

% assign(c1, Course). or assign(c1, cse101).
assign(Room,Course) :- course(Course,Teacher,CourseCapacity,_,_,Students,_), % find course
                       instructor(Teacher,_,Equipment), % find instructor of course for equipment
                       (isThereHandicapped(Students)  ->   % checks if any handicapped students are there
                         (Equipment = 'nothing' -> room(Room,RoomCapacity,_,_,yes) ; room(Room,RoomCapacity,_,Equipment,yes))
                         ;
                         (Equipment = 'nothing' -> room(Room,RoomCapacity,_,_,_) ; room(Room,RoomCapacity,_,Equipment,_))),
    				   RoomCapacity >= CourseCapacity.   % checks room capacity

% finds conflict between 2 courses by looking their hours and checking one by one with other courses hours
% findConflicts(cse341,Course2).
findConflicts(Course1,Course2) :- occupancy(_,Course1,Hours1), occupancy(_,Course2,Hours2), (search(Hours1,Hours2) -> true ; search(Hours1,Hours2)).  

% compares one courses hours with other courses hours of student to find if conflicts
checkOverlap(CourseId,[Head|Tail]) :- (  findConflicts(CourseId,Head) -> true ; checkOverlap(CourseId,Tail)).

% checks if an element is in element list according to inputs
search([First|_], List):- ( memberchk(First,List)  -> true ; search(First,List)) .  
search([_|Last],List):- search(Last,List). 


% checks if capacity of room/course is exceeded
isCapacitySmaller(FN,SN) :- length(FN, LEN), LEN < SN. % check given capacity is smaller than other.

%checks if in student list are there handicapped student
isThereHandicapped([Head|Tail]) :- student(Head,_,H), (H = 'yes' ->  true ; isThereHandicapped(Tail)). 
    								
