package com.dpw.runner.shipment.services.mapper;

import com.dpw.runner.shipment.services.dto.request.JobRequest;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.response.JobResponse;
import com.dpw.runner.shipment.services.entity.Jobs;
import com.dpw.runner.shipment.services.entity.Parties;
import org.mapstruct.*;

@Mapper(
        uses = JsonNullableMapper.class,
        nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE,
        componentModel = MappingConstants.ComponentModel.SPRING,
        unmappedTargetPolicy = ReportingPolicy.IGNORE
)
public interface JobsMapper {

    Jobs map(JobRequest req);
    JobResponse map(Jobs entity);
    Parties map(PartiesRequest req);
    @InheritConfiguration
    void update(JobRequest update, @MappingTarget Jobs destination);
}
