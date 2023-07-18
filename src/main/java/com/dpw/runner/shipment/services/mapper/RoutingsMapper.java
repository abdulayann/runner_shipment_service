package com.dpw.runner.shipment.services.mapper;

import com.dpw.runner.shipment.services.dto.request.RoutingsRequest;
import com.dpw.runner.shipment.services.dto.response.RoutingsResponse;
import com.dpw.runner.shipment.services.entity.Routings;
import org.mapstruct.*;

@Mapper(
        uses = JsonNullableMapper.class,
        nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE,
        componentModel = MappingConstants.ComponentModel.SPRING,
        unmappedTargetPolicy = ReportingPolicy.IGNORE
)
public interface RoutingsMapper {

    Routings map(RoutingsRequest req);
    RoutingsResponse map(Routings entity);
    RoutingsRequest getRequest(Routings entity);

    @InheritConfiguration
    void update(RoutingsRequest update, @MappingTarget Routings destination);
}
