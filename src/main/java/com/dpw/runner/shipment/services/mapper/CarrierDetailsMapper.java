package com.dpw.runner.shipment.services.mapper;

import com.dpw.runner.shipment.services.dto.request.CarrierDetailRequest;
import com.dpw.runner.shipment.services.dto.response.CarrierDetailResponse;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import org.mapstruct.*;

@Mapper(
        uses = JsonNullableMapper.class,
        nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE,
        componentModel = MappingConstants.ComponentModel.SPRING,
        unmappedTargetPolicy = ReportingPolicy.IGNORE
)
public interface CarrierDetailsMapper {

    CarrierDetails map(CarrierDetailRequest req);
    CarrierDetailResponse map(CarrierDetails entity);
    CarrierDetailRequest getRequest(CarrierDetails entity);

    @InheritConfiguration
    void update(CarrierDetailRequest update, @MappingTarget CarrierDetails destination);
}
