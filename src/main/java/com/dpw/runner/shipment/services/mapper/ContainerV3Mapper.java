package com.dpw.runner.shipment.services.mapper;

import com.dpw.runner.shipment.services.dto.patchrequest.ContainerV3PatchRequest;
import com.dpw.runner.shipment.services.dto.request.ContainerV3Request;
import org.mapstruct.*;

@Mapper(
        uses = {JsonNullableMapper.class},
        nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE,
        componentModel = MappingConstants.ComponentModel.SPRING,
        unmappedTargetPolicy = ReportingPolicy.IGNORE
)
public interface ContainerV3Mapper {

    @InheritConfiguration
    void update(ContainerV3PatchRequest update, @MappingTarget ContainerV3Request destination);

}
