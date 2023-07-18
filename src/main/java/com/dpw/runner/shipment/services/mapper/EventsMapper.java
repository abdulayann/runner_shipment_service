package com.dpw.runner.shipment.services.mapper;

import com.dpw.runner.shipment.services.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.dto.response.EventsResponse;
import com.dpw.runner.shipment.services.entity.Events;
import org.mapstruct.*;

@Mapper(
        uses = JsonNullableMapper.class,
        nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE,
        componentModel = MappingConstants.ComponentModel.SPRING,
        unmappedTargetPolicy = ReportingPolicy.IGNORE
)
public interface EventsMapper {

    Events map(EventsRequest req);
    EventsResponse map(Events entity);
    EventsRequest getRequest(Events entity);

    @InheritConfiguration
    void update(EventsRequest update, @MappingTarget Events destination);
}
