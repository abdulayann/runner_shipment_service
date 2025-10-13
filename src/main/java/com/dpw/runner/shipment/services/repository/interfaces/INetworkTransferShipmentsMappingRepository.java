package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.NetworkTransferShipmentsMapping;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Repository
@Generated
public interface INetworkTransferShipmentsMappingRepository extends MultiTenancyRepository<NetworkTransferShipmentsMapping> {
    List<NetworkTransferShipmentsMapping> findByNetworkTransferId(Long networkTransferId);

    List<NetworkTransferShipmentsMapping> findByEntityTypeAndEntityNumber(String entityType, String entityNumber);

    @Modifying
    @Transactional
    @Query("DELETE FROM NetworkTransferShipmentsMapping n WHERE n.networkTransferId = :networkTransferId")
    void deleteByNetworkTransferId(@Param("networkTransferId") Long networkTransferId);

    @Query("SELECT n.shipmentNumber FROM NetworkTransferShipmentsMapping n WHERE n.networkTransferId = :networkTransferId")
    List<String> findShipmentNumbersByNetworkTransferId(@Param("networkTransferId") Long networkTransferId);

    @Modifying
    @Transactional
    @Query("DELETE FROM NetworkTransferShipmentsMapping n WHERE n.networkTransferId = :networkTransferId AND n.shipmentNumber IN :shipmentNumbers")
    void deleteByNetworkTransferIdAndShipmentNumbers(
            @Param("networkTransferId") Long networkTransferId,
            @Param("shipmentNumbers") List<String> shipmentNumbers);

}
