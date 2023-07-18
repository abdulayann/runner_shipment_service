package com.dpw.runner.shipment.services.mapper;

import com.dpw.runner.shipment.services.dto.request.PackingRequest;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.entity.Packing;
import org.mapstruct.*;

@Mapper(
        uses = JsonNullableMapper.class,
        nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE,
        componentModel = MappingConstants.ComponentModel.SPRING,
        unmappedTargetPolicy = ReportingPolicy.IGNORE
)
public interface PackingsMapper {

    Packing map(PackingRequest req);
    PackingResponse map(Packing entity);
    PackingRequest getRequest(Packing entity);

    @InheritConfiguration
    void update(PackingRequest update, @MappingTarget Packing destination);
}
