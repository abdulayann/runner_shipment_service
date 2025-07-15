package com.dpw.runner.shipment.services.migration.strategy.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IPickupDeliveryDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.response.PickupDeliveryDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.migration.entity.ShipmentBackupEntity;
import com.dpw.runner.shipment.services.migration.repository.interfaces.ShipmentBackupRepository;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.BackupRestoreStrategy;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import lombok.extern.slf4j.Slf4j;
import org.hibernate.Hibernate;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@Slf4j
public class ShipmentStrategy implements BackupRestoreStrategy {

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private IPickupDeliveryDetailsDao pickupDeliveryDetailsDao;

    @Autowired
    private ShipmentBackupRepository shipmentBackupRepository;

    @Override
    public void backup(Integer tenantId) {
        int count = 0;
        try {
            List<ShipmentDetails> shipmentDetailsList = shipmentDao.findAllByTenantId(tenantId);
            log.info(String.valueOf(shipmentDetailsList.size()));
            for (ShipmentDetails details : shipmentDetailsList) {
                count++;
                ShipmentBackupEntity backupEntity = new ShipmentBackupEntity();
                backupEntity.setTenantId(tenantId);
                backupEntity.setShipmentId(details.getShipmentId());
                backupEntity.setShipmentGuid(details.getGuid());
                ShipmentDetailsResponse response = modelMapper.map(details, ShipmentDetailsResponse.class);
                backupEntity.setShipmentDetail(response);

                List<PickupDeliveryDetails> pickupDeliveryDetails = pickupDeliveryDetailsDao.findByShipmentId(details.getId());

                List<PickupDeliveryDetailsResponse> dtoList = modelMapper.map(
                        pickupDeliveryDetails,
                        new TypeToken<List<PickupDeliveryDetailsResponse>>() {}.getType()
                );
                backupEntity.setPickupDeliveryDetail(dtoList);
                shipmentBackupRepository.save(backupEntity);
            }
        }   catch (Exception e) {
            log.info(String.valueOf(count));
            log.info(e.getMessage());
        }
    }

    @Override
    public void delete(Integer tenantId) {
        shipmentBackupRepository.deleteByTenantId(tenantId);
    }

    @Override
    public void restore(Integer tenantId) {

    }
}
