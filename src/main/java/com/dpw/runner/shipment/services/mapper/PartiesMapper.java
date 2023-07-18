package com.dpw.runner.shipment.services.mapper;

import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.entity.Parties;
import org.mapstruct.*;

@Mapper(
        uses = JsonNullableMapper.class,
        nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE,
        componentModel = MappingConstants.ComponentModel.SPRING,
        unmappedTargetPolicy = ReportingPolicy.IGNORE
)
public interface PartiesMapper {

    Parties map(PartiesRequest req);
    PartiesResponse map(Parties entity);
    PartiesRequest getRequest(Parties entity);

    @InheritConfiguration
    void update(PartiesRequest update, @MappingTarget Parties destination);
}
