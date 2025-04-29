package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.TenantProducts;
import com.dpw.runner.shipment.services.entity.enums.ProductProcessTypes;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.GetNextNumberHelper;
import com.dpw.runner.shipment.services.utils.ProductIdentifierUtility;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import java.util.List;
import java.util.Optional;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@Component
@Slf4j
public class ShipmentsV3Util {

    private IShipmentSettingsDao shipmentSettingsDao;
    private GetNextNumberHelper getNextNumberHelper;
    private IV1Service v1Service;
    private ProductIdentifierUtility productEngine;
    private JsonHelper jsonHelper;
    private IShipmentDao shipmentDao;

    @Autowired
    public ShipmentsV3Util(IShipmentSettingsDao shipmentSettingsDao,
                           GetNextNumberHelper getNextNumberHelper,
                           IV1Service v1Service,
                           ProductIdentifierUtility productEngine,
                           IShipmentDao shipmentDao,
                           JsonHelper jsonHelper) {
        this.shipmentSettingsDao = shipmentSettingsDao;
        this.getNextNumberHelper = getNextNumberHelper;
        this.v1Service = v1Service;
        this.productEngine = productEngine;
        this.shipmentDao = shipmentDao;
        this.jsonHelper = jsonHelper;
    }

    public String generateShipmentId(ShipmentDetails shipmentDetails) {
        Optional<ShipmentSettingsDetails> shipmentSettingsOptional = shipmentSettingsDao.findByTenantId(TenantContext.getCurrentTenant());
        String shipmentId = "";
        boolean flag = true;
        int counter = 1;
        while(flag) {
            ListCommonRequest listRequest = constructListCommonRequest(Constants.SHIPMENT_ID, shipmentId, "=");
            Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listRequest, ShipmentDetails.class);
            Page<ShipmentDetails> shipments = shipmentDao.findAll(pair.getLeft(), pair.getRight());

            if(!shipmentId.isEmpty() && shipments.getTotalElements() == 0)
                flag = false;
            else {
                log.info("CR-ID {} || Inside generateShipmentId: with shipmentID: {} | counter: {}", LoggerHelper.getRequestIdFromMDC(), shipmentId, counter++);
                if(shipmentSettingsOptional.isPresent() && Boolean.TRUE.equals(shipmentSettingsOptional.get().getCustomisedSequence())) {
                    try{
                        shipmentId = getCustomizedShipmentProcessNumber(ProductProcessTypes.ShipmentNumber, shipmentDetails);
                    } catch (Exception ignored) {
                        log.error("Exception during common sequence {}", ignored.getMessage());
                        log.error("Exception occurred for common sequence {}", ignored.getStackTrace());
                        shipmentId = Constants.SHIPMENT_ID_PREFIX + getShipmentsSerialNumber();
                    }
                }
                ShipmentSettingsDetails shipmentSettings = shipmentSettingsOptional.orElse(null);
                shipmentId = getShipmentId(shipmentId, shipmentSettings);
            }
        }
        return shipmentId;
    }

    private String getCustomizedShipmentProcessNumber(ProductProcessTypes productProcessType, ShipmentDetails currentShipment) throws RunnerException {
        List<TenantProducts> tenantProducts = productEngine.populateEnabledTenantProducts();
        // to check the commmon sequence
        var sequenceNumber = productEngine.getCommonSequenceNumber(currentShipment.getTransportMode(), ProductProcessTypes.Consol_Shipment_TI);
        if (sequenceNumber != null && !sequenceNumber.isEmpty()) {
            return sequenceNumber;
        }
        var identifiedProduct = productEngine.identifyProduct(currentShipment, tenantProducts);
        if (identifiedProduct == null) {
            return "";
        }
        var sequenceSettings = getNextNumberHelper.getProductSequence(identifiedProduct.getId(), productProcessType);
        if(sequenceSettings == null) {
            sequenceSettings = productEngine.getShipmentProductWithOutContainerType(currentShipment, productProcessType, tenantProducts);
            if (sequenceSettings == null) {
                // get default product type for shipment
                var defaultProduct = productEngine.getDefaultShipmentProduct(tenantProducts);
                if (defaultProduct == null || identifiedProduct == defaultProduct) {
                    return "";
                }
                sequenceSettings = getNextNumberHelper.getProductSequence(defaultProduct.getId(), productProcessType);
                if (sequenceSettings == null) {
                    return "";
                }
            }
        }
        String prefix = sequenceSettings.getPrefix() == null ? "" : sequenceSettings.getPrefix();
        var user = UserContext.getUser();
        return getNextNumberHelper.generateCustomSequence(sequenceSettings, prefix, user.TenantId, true, null, false);
    }

    private String getShipmentsSerialNumber() {
        // Moving this responsibility to v1 sequence table to avoid syncing overhead
        return v1Service.getShipmentSerialNumber();
    }

    private String getShipmentId(String shipmentId, ShipmentSettingsDetails shipmentSettings) {
        if(StringUtility.isEmpty(shipmentId)) {
            if(shipmentSettings != null) {
                log.info("CR-ID {} || no common sequence found and shipment settings data is: {}",
                        LoggerHelper.getRequestIdFromMDC(),
                        jsonHelper.convertToJson(shipmentSettings));
            }
            log.info("CR-ID {} || no common sequence found", LoggerHelper.getRequestIdFromMDC());
            shipmentId = Constants.SHIPMENT_ID_PREFIX + getShipmentsSerialNumber();
        }
        return shipmentId;
    }
}
