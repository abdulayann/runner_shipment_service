package com.dpw.runner.shipment.services.mapper;

import static org.junit.jupiter.api.Assertions.*;

import com.dpw.runner.shipment.services.dto.request.BookingCarriageRequest;
import com.dpw.runner.shipment.services.entity.BookingCarriage;
import org.junit.jupiter.api.Test;
import org.mapstruct.factory.Mappers;
import org.openapitools.jackson.nullable.JsonNullable;

class BookingCarriageMapperTest {

  @Test
  void test1() {
    BookingCarriageMapper mapper = Mappers.getMapper(BookingCarriageMapper.class);
    BookingCarriageRequest req = BookingCarriageRequest.builder().bookingId(1L).vessel("vessel").carriageMode("CM")
            .carriageType(null).build();
    BookingCarriage existing = BookingCarriage.builder().bookingId(1L).vessel("v1").carriageMode("cm").carriageType("ctp").build();
    BookingCarriage expected = BookingCarriage.builder().bookingId(1L).vessel("vessel").carriageMode("cm").carriageType("ctp").build();
    mapper.update(req, existing);
    assertEquals(expected.getBookingId(), existing.getBookingId());
    assertEquals(expected.getVessel(), existing.getVessel());
    assertEquals(expected.getCarriageMode(), existing.getCarriageMode());
    assertEquals(expected.getCarriageType(), existing.getCarriageType());
  }
}
