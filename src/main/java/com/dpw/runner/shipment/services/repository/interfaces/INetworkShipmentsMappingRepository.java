package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.NetworkShipmentsMapping;
import com.dpw.runner.shipment.services.entity.NetworkTransfer;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;

@Repository
@Generated
public interface INetworkShipmentsMappingRepository extends MultiTenancyRepository<NetworkShipmentsMapping> {
    Optional<NetworkShipmentsMapping> findByNetworkTransferAndShipmentNumber(NetworkTransfer networkTransfer, String shipmentNumber);

    List<NetworkShipmentsMapping> findByNetworkTransfer(NetworkTransfer networkTransfer);

    List<NetworkShipmentsMapping> findByEntityTypeAndEntityNumber(String entityType, String entityNumber);

    @Modifying
    @Transactional
    @Query("DELETE FROM NetworkShipmentsMapping n WHERE n.networkTransfer = :networkTransfer")
    void deleteByNetworkTransfer(@Param("networkTransfer") NetworkTransfer networkTransfer);

}
