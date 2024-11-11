package com.dpw.runner.shipment.services.mapper;

import com.dpw.runner.shipment.services.dto.patchrequest.BookingCarriagePatchRequest;
import com.dpw.runner.shipment.services.dto.request.BookingCarriageRequest;
import com.dpw.runner.shipment.services.dto.response.BookingCarriageResponse;
import com.dpw.runner.shipment.services.entity.BookingCarriage;
import org.mapstruct.*;

@Mapper(
        uses = JsonNullableMapper.class,
        nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE,
        componentModel = MappingConstants.ComponentModel.SPRING
)
public interface BookingCarriageMapper {

    BookingCarriage map(BookingCarriageRequest req);
    BookingCarriageResponse map(BookingCarriage entity);
    BookingCarriageRequest getRequest(BookingCarriage entity);

    @InheritConfiguration
    void update(BookingCarriagePatchRequest update, @MappingTarget BookingCarriage destination);
}
