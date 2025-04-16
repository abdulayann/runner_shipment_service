package com.dpw.runner.shipment.services.mapper;

import com.dpw.runner.shipment.services.dto.patchrequest.ShipmentSettingsPatchRequest;
import com.dpw.runner.shipment.services.dto.request.HawbLockSettingsRequest;
import com.dpw.runner.shipment.services.dto.request.HblLockSettingsRequest;
import com.dpw.runner.shipment.services.dto.request.MawbLockSettingsRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentSettingRequest;
import com.dpw.runner.shipment.services.dto.response.ShipmentSettingsDetailsResponse;
import com.dpw.runner.shipment.services.entity.HawbLockSettings;
import com.dpw.runner.shipment.services.entity.HblLockSettings;
import com.dpw.runner.shipment.services.entity.MawbLockSettings;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import org.mapstruct.*;

@Mapper(
        uses = {JsonNullableMapper.class, AuditLogMapper.class},
        nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE,
        componentModel = MappingConstants.ComponentModel.SPRING,
        unmappedTargetPolicy = ReportingPolicy.IGNORE
)
public interface ShipmentSettingsMapper {
    ShipmentSettingsDetails map(ShipmentSettingRequest req);
    ShipmentSettingsDetailsResponse map(ShipmentSettingsDetails entity);
    ShipmentSettingRequest getRequest(ShipmentSettingsDetails shipmentSettingDetails);

    // Mappers for nested entities
    HawbLockSettings map(HawbLockSettingsRequest req);
    MawbLockSettings map(MawbLockSettingsRequest req);
    HblLockSettings map(HblLockSettingsRequest req);

    @InheritConfiguration
    void update(ShipmentSettingsPatchRequest update, @MappingTarget ShipmentSettingsDetails destination);
}
