package com.dpw.runner.shipment.services.mapper;

import com.dpw.runner.shipment.services.dto.request.ReferenceNumbersRequest;
import com.dpw.runner.shipment.services.dto.response.ReferenceNumbersResponse;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import org.mapstruct.*;

@Mapper(
        uses = JsonNullableMapper.class,
        nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE,
        componentModel = MappingConstants.ComponentModel.SPRING,
        unmappedTargetPolicy = ReportingPolicy.IGNORE
)
public interface ReferenceNumbersMapper {

    ReferenceNumbers map(ReferenceNumbersRequest req);
    ReferenceNumbersResponse map(ReferenceNumbers entity);
    ReferenceNumbersRequest getRequest(ReferenceNumbers entity);

    @InheritConfiguration
    void update(ReferenceNumbersRequest update, @MappingTarget ReferenceNumbers destination);
}
