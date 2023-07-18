package com.dpw.runner.shipment.services.mapper;

import com.dpw.runner.shipment.services.dto.request.TruckDriverDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.TruckDriverDetailsResponse;
import com.dpw.runner.shipment.services.entity.TruckDriverDetails;
import org.mapstruct.*;

@Mapper(
        uses = JsonNullableMapper.class,
        nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE,
        componentModel = MappingConstants.ComponentModel.SPRING,
        unmappedTargetPolicy = ReportingPolicy.IGNORE
)
public interface TruckDriverDetailsMapper {

    TruckDriverDetails map(TruckDriverDetailsRequest req);
    TruckDriverDetailsResponse map(TruckDriverDetails entity);

    @InheritConfiguration
    void update(TruckDriverDetailsRequest update, @MappingTarget TruckDriverDetails destination);
}
