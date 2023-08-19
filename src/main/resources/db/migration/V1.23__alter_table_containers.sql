alter table if exists containers
add column if not exists booking_id bigint;

alter table if exists packing
add column if not exists booking_id bigint;

alter table if exists routings
add column if not exists booking_id bigint;

alter table if exists containers
add constraint fk_containers_booking
foreign key (booking_id) references customer_booking(id);

alter table if exists packing
add constraint fk_packing_booking
foreign key (booking_id) references customer_booking(id);

alter table if exists routings
add constraint fk_routings_booking
foreign key (booking_id) references customer_booking(id);