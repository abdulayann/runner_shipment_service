package com.dpw.runner.shipment.services.mapper;

import com.dpw.runner.shipment.services.dto.request.AdditionalDetailRequest;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.response.AdditionalDetailResponse;
import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.entity.Parties;
import org.mapstruct.*;
import org.openapitools.jackson.nullable.JsonNullable;

@Mapper(
        uses = JsonNullableMapper.class,
        nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE,
        componentModel = MappingConstants.ComponentModel.SPRING,
        unmappedTargetPolicy = ReportingPolicy.IGNORE
)
public interface AdditionalDetailsMapper {

    AdditionalDetails map(AdditionalDetailRequest req);
    AdditionalDetailResponse map(AdditionalDetails entity);

    Parties map(PartiesRequest req);

    @InheritConfiguration
    void update(AdditionalDetailRequest update, @MappingTarget AdditionalDetails destination);
}
