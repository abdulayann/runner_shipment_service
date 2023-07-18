package com.dpw.runner.shipment.services.mapper;

import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.request.PickupDeliveryDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.PickupDeliveryDetailsResponse;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import org.mapstruct.*;

@Mapper(
        uses = JsonNullableMapper.class,
        nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE,
        componentModel = MappingConstants.ComponentModel.SPRING,
        unmappedTargetPolicy = ReportingPolicy.IGNORE
)
public interface PickupDeliveryDetailsMapper {

    PickupDeliveryDetails map(PickupDeliveryDetailsRequest req);
    PickupDeliveryDetailsResponse map(PickupDeliveryDetails entity);
    Parties map(PartiesRequest req);

    @InheritConfiguration
    void update(PickupDeliveryDetailsRequest update, @MappingTarget PickupDeliveryDetails destination);
}
