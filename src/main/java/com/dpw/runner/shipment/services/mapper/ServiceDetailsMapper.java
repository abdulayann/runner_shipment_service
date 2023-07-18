package com.dpw.runner.shipment.services.mapper;

import com.dpw.runner.shipment.services.dto.request.ServiceDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.ServiceDetailsResponse;
import com.dpw.runner.shipment.services.entity.ServiceDetails;
import org.mapstruct.*;

@Mapper(
        uses = JsonNullableMapper.class,
        nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE,
        componentModel = MappingConstants.ComponentModel.SPRING,
        unmappedTargetPolicy = ReportingPolicy.IGNORE
)
public interface ServiceDetailsMapper {

    ServiceDetails map(ServiceDetailsRequest req);
    ServiceDetailsResponse map(ServiceDetails entity);
//    ServiceDetailsRequest getRequest(ServiceDetails entity);

    @InheritConfiguration
    void update(ServiceDetailsRequest update, @MappingTarget ServiceDetails destination);
}
