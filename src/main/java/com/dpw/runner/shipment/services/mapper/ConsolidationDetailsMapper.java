package com.dpw.runner.shipment.services.mapper;

import com.dpw.runner.shipment.services.dto.patchrequest.ConsolidationPatchRequest;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.TriangulationPartnerResponse;
import com.dpw.runner.shipment.services.entity.*;
import org.mapstruct.*;

import java.util.List;

@Mapper(
        uses = JsonNullableMapper.class,
        nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE,
        componentModel = MappingConstants.ComponentModel.SPRING,
        unmappedTargetPolicy = ReportingPolicy.IGNORE
)
public interface ConsolidationDetailsMapper {

    ConsolidationDetails map(ConsolidationDetailsRequest req);

    ConsolidationDetailsResponse map(ConsolidationDetails entity);

    ConsolidationDetailsRequest getRequest(ConsolidationDetails consolidationDetails);

    //Child entities
    Parties map(PartiesRequest req);

    Allocations map(AllocationsRequest req);

    AchievedQuantities map(AchievedQuantitiesRequest req);

    ArrivalDepartureDetails map(ArrivalDepartureDetailsRequest req);

    TruckDriverDetails map(TruckDriverDetailsRequest req);

    Routings map(RoutingsRequest req);

    ReferenceNumbers map(ReferenceNumbersRequest req);

    Packing map(PackingRequest req);

    FileRepo map(FileRepoRequest req);

    Events map(EventsRequest req);

    CarrierDetails map(CarrierDetailRequest req);

    Jobs map(JobRequest req);

    Containers map(ContainerRequest req);

    List<TriangulationPartner> map(List<TriangulationPartnerRequest> req);

    TriangulationPartner map(TriangulationPartnerRequest req);

    TriangulationPartnerResponse map(TriangulationPartner partner);

    @InheritConfiguration
    void update(ConsolidationPatchRequest update, @MappingTarget ConsolidationDetails destination);

}
