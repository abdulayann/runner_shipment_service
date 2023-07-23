package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationShipmentsMappingDao;
import com.dpw.runner.shipment.services.repository.interfaces.IConsolShipLinkRepository;
import com.dpw.runner.shipment.services.entity.ConsolShipLinkDetails;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;
import java.util.HashSet;

@Repository
public class ConsolidationShipmentsMappingDao implements IConsolidationShipmentsMappingDao {
    @Autowired
    private IConsolShipLinkRepository consolShipLinkRepository;

    private List<ConsolShipLinkDetails> findByConsolidationId(Long consolidationId) {
        return consolShipLinkRepository.findByConsolidationId(consolidationId);
    }

    private List<ConsolShipLinkDetails> findByShipmentId(Long shipmentId) {
        return consolShipLinkRepository.findByShipmentId(shipmentId);
    }

    private ConsolShipLinkDetails save(ConsolShipLinkDetails consolShipLinkDetails) {
        return consolShipLinkRepository.save(consolShipLinkDetails);
    }

    private void delete(ConsolShipLinkDetails shipmentsContainersMapping) {
        consolShipLinkRepository.delete(shipmentsContainersMapping);
    }

    @Override
    public void assignShipments(Long consolidationId, List<Long> shipIds) {
        List<ConsolShipLinkDetails> mappings = findByConsolidationId(consolidationId);
        HashSet<Long> shipmentIds = new HashSet<>(shipIds);
        if(mappings != null && mappings.size() > 0)
        {
            for (ConsolShipLinkDetails shipmentsContainersMappings: mappings) {
                if(shipmentIds.contains(shipmentsContainersMappings.getShipmentId())) {
                    shipmentIds.remove(shipmentsContainersMappings.getShipmentId());
                }
            }
        }
        if(!shipmentIds.isEmpty()) {
            for (Long id: shipmentIds) {
                ConsolShipLinkDetails entity = new ConsolShipLinkDetails();
                entity.setShipmentId(id);
                entity.setConsolidationId(consolidationId);
                save(entity);
            }
        }
    }

    @Override
    public void detachShipments(Long consolidationId, List<Long> shipIds) {
        List<ConsolShipLinkDetails> mappings = findByConsolidationId(consolidationId);
        HashSet<Long> shipmentIds = new HashSet<>(shipIds);
        List<ConsolShipLinkDetails> deleteMappings = new ArrayList<>();
        if(mappings != null && mappings.size() > 0)
        {
            for (ConsolShipLinkDetails shipmentsContainersMappings: mappings) {
                if(shipmentIds.contains(shipmentsContainersMappings.getShipmentId())) {
                    deleteMappings.add(shipmentsContainersMappings);
                }
            }
        }
        if(!deleteMappings.isEmpty()) {
            for (ConsolShipLinkDetails shipmentsContainersMapping: deleteMappings) {
                delete(shipmentsContainersMapping);
            }
        }
    }

    @Override
    public void updateShipmentsMappings(Long consolidationId, List<Long> shipIds) {
        List<ConsolShipLinkDetails> mappings = findByConsolidationId(consolidationId);
        HashSet<Long> shipmentIds = new HashSet<>(shipIds);
        List<ConsolShipLinkDetails> deleteMappings = new ArrayList<>();
        for (ConsolShipLinkDetails shipmentsContainersMapping: mappings) {
            if(shipmentIds.contains(shipmentsContainersMapping.getShipmentId())) {
                shipmentIds.remove(shipmentsContainersMapping.getShipmentId());
            }
            else {
                deleteMappings.add(shipmentsContainersMapping);
            }
        }
        if(!shipmentIds.isEmpty()) {
            for (Long shipmentId: shipmentIds) {
                ConsolShipLinkDetails entity = new ConsolShipLinkDetails();
                entity.setShipmentId(shipmentId);
                entity.setConsolidationId(consolidationId);
                save(entity);
            }
        }
        if(!deleteMappings.isEmpty()) {
            for (ConsolShipLinkDetails mapping: deleteMappings) {
                delete(mapping);
            }
        }
    }
}
