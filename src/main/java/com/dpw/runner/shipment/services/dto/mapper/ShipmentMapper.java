package com.dpw.runner.shipment.services.dto.mapper;

import com.dpw.runner.shipment.services.dto.response.ShipmentExcelExportResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentListResponse;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(
        uses = {CarrierDetailsMapper.class, PartiesResponseMapper.class, PickupDeliveryDetailsListResponseMapper.class, AdditionalDetailsListResponseMapper.class}
)
public interface ShipmentMapper {

    ShipmentMapper INSTANCE = Mappers.getMapper(ShipmentMapper.class);

    ShipmentListResponse toShipmentListResponse(ShipmentDetails shipmentDetails);

    List<ShipmentListResponse> toShipmentListResponses(List<ShipmentDetails> shipmentDetails);

    List<ShipmentExcelExportResponse> toShipmentExportListResponses(List<ShipmentDetails> shipmentDetails);
}