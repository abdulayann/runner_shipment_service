package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@Component
@Slf4j
public class ConsolidationV3Util {

    private CommonUtils commonUtils;
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    private IShipmentDao shipmentDao;

    @Autowired
    public ConsolidationV3Util(CommonUtils commonUtils, IConsoleShipmentMappingDao consoleShipmentMappingDao, IShipmentDao shipmentDao) {
        this.commonUtils = commonUtils;
        this.consoleShipmentMappingDao = consoleShipmentMappingDao;
        this.shipmentDao = shipmentDao;
    }

    /**
     * Checks if a consolidation is eligible for CFS (Container Freight Station) validation. Criteria: - Transport mode must be SEA - Shipment type must be EXPORT - LCL
     * consolidation setting must be enabled in context
     *
     * @param consolidationDetails the consolidation to validate
     * @return true if eligible for CFS validation, false otherwise
     */
    public boolean checkConsolidationEligibleForCFSValidation(ConsolidationDetails consolidationDetails) {
        // Must be SEA transport mode
        if (!Constants.TRANSPORT_MODE_SEA.equals(consolidationDetails.getTransportMode())) {
            return false;
        }

        // Must be an Export shipment
        if (!Constants.DIRECTION_EXP.equals(consolidationDetails.getShipmentType())) {
            return false;
        }

        // LCL consolidation setting must be enabled
        return Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEnableLclConsolidation());
    }

    /**
     * Retrieves a list of shipments associated with a given consolidation ID.
     * <p>
     * This method fetches mappings from the `ConsoleShipmentMapping` table, extracts shipment IDs, constructs a dynamic list query, and returns the corresponding
     * `ShipmentDetails`.
     *
     * @param consoleId The ID of the consolidation whose shipments need to be fetched
     * @return List of linked `ShipmentDetails`
     */
    public List<ShipmentDetails> getShipmentsList(Long consoleId) {
        // Fetch mapping records between the consolidation and its linked shipments
        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(consoleId);

        // Extract shipment IDs from the mapping
        List<Long> shipmentIdList = consoleShipmentMappings.stream()
                .map(ConsoleShipmentMapping::getShipmentId)
                .toList();

        // Build a dynamic list request using "IN" operation on shipment IDs
        ListCommonRequest listReq = constructListCommonRequest("id", shipmentIdList, "IN");

        // Create JPA Specification and pagination info from the request
        Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listReq, ShipmentDetails.class);

        // Execute the paginated query using the specification
        Page<ShipmentDetails> page = shipmentDao.findAll(pair.getLeft(), pair.getRight());

        // Return the list of shipment details
        return new ArrayList<>(page.getContent());
    }

}
