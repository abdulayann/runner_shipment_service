package com.dpw.runner.shipment.services.mapper;

import com.dpw.runner.shipment.services.dto.patchrequest.ShipmentPatchRequest;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.TriangulationPartnerResponse;
import com.dpw.runner.shipment.services.entity.*;
import org.mapstruct.*;

import java.util.List;

@Mapper(
        uses = {JsonNullableMapper.class, AuditLogMapper.class},
        nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE,
        componentModel = MappingConstants.ComponentModel.SPRING,
        unmappedTargetPolicy = ReportingPolicy.IGNORE
)
public interface ShipmentDetailsMapper {

    ShipmentDetails map(ShipmentRequest req);
    ShipmentDetailsResponse map(ShipmentDetails entity);
    ShipmentRequest getRequest(ShipmentDetails shipmentDetails);

    // Mappers for nested entities
    Parties map(PartiesRequest req);
    AdditionalDetails map(AdditionalDetailRequest req);
    PickupDeliveryDetails map(PickupDeliveryDetailsRequest req);
    Notes map(NotesRequest req);
    TruckDriverDetails map(TruckDriverDetailsRequest req);
    ServiceDetails map(ServiceDetailsRequest req);
    Routings map(RoutingsRequest req);
    ReferenceNumbers map(ReferenceNumbersRequest req);
    Packing map(PackingRequest req);
    FileRepo map(FileRepoRequest req);
    Events map(EventsRequest req);
    ELDetails map(ELDetailsRequest req);
    BookingCarriage map(BookingCarriageRequest req);
    CarrierDetails map(CarrierDetailRequest req);
    Jobs map(JobRequest req);
    Containers map(ContainerRequest req);
    List<TriangulationPartner> map(List<TriangulationPartnerRequest> req);
    TriangulationPartner map(TriangulationPartnerRequest req);
    TriangulationPartnerResponse map(TriangulationPartner partner);

    @InheritConfiguration
    void update(ShipmentPatchRequest update, @MappingTarget ShipmentDetails destination);
}
