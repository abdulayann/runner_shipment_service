package com.dpw.runner.shipment.services.mapper;

import com.dpw.runner.shipment.services.dto.request.ELDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.ELDetailsResponse;
import com.dpw.runner.shipment.services.entity.ELDetails;
import org.mapstruct.*;

@Mapper(
        uses = JsonNullableMapper.class,
        nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE,
        componentModel = MappingConstants.ComponentModel.SPRING,
        unmappedTargetPolicy = ReportingPolicy.IGNORE
)
public interface ELDetailsMapper {

    ELDetails map(ELDetailsRequest req);
    ELDetailsResponse map(ELDetails entity);

    @InheritConfiguration
    void update(ELDetailsRequest update, @MappingTarget ELDetails destination);
}
