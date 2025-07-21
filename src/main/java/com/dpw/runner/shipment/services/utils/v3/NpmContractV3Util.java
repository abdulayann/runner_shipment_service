package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.commons.constants.NPMConstants;
import com.dpw.runner.shipment.services.dto.response.ListContractResponse;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.*;

@Component
@Slf4j
public class NpmContractV3Util {
    private final MasterDataUtils masterDataUtils;

    @Autowired
    public NpmContractV3Util(MasterDataUtils masterDataUtils) {
        this.masterDataUtils = masterDataUtils;
    }

    public CarrierDetails createCarrierDetails(ListContractResponse.ContractResponse contract) {
        var meta = contract.getMeta();
        return CarrierDetails.builder()
                .origin(contract.getOrigin())
                .destination(contract.getDestination())
                .originPort(meta != null ? meta.getPol() : null)
                .destinationPort(meta != null ? meta.getPod() : null)
                .shippingLine(getCarrier(contract))
                .minTransitHours(meta != null ? meta.getMinTransitHours() : null)
                .maxTransitHours(meta != null ? meta.getMaxTransitHours() : null)
                .build();
    }

    private String getCarrier(ListContractResponse.ContractResponse contract) {
        List<String> carrierCodes = contract.getCarrier_codes();
        if (carrierCodes == null) return null;

        carrierCodes = new ArrayList<>(carrierCodes);
        carrierCodes.removeAll(List.of(NPMConstants.ANY, NPMConstants.SQSN));

        if (carrierCodes.isEmpty()) return null;

        Map<String, EntityTransferCarrier> map = masterDataUtils.fetchInBulkCarriersBySCACCode(carrierCodes);
        return Optional.ofNullable(map.get(carrierCodes.get(0)))
                .map(EntityTransferCarrier::getItemValue)
                .orElse(null);
    }

    public void setFilterParams(ListContractResponse.FilterParams filters, PackingV3Request request) {
        if (filters == null) return;
        if (filters.getCargo_type() != null && !filters.getCargo_type().isEmpty()) {
            request.setPacksType(filters.getCargo_type().get(0));
        }
        if (filters.getCommodity() != null && !filters.getCommodity().isEmpty()) {
            request.setCommodityGroup(filters.getCommodity().get(0));
        }
    }

    public void setMetaData(ListContractResponse.ContractUsage usage, PackingV3Request request) {
        var meta = usage.getMeta();
        if (meta == null) return;
        var loadAttributes = meta.getLoad_attributes();
        request.setPacks(loadAttributes.getQuantity().toString());
        request.setWeight(Objects.isNull(loadAttributes.getWeight()) ? BigDecimal.ZERO : loadAttributes.getWeight());
        request.setWeightUnit(loadAttributes.getWeight_uom());
        request.setVolume(Objects.isNull(loadAttributes.getVolume()) ? BigDecimal.ZERO : loadAttributes.getVolume());
        request.setVolumeUnit(loadAttributes.getVolume_uom());
        request.setIsDimension(false);
        var dimensions = loadAttributes.getDimensions();
        if (dimensions != null) {
            setDimensions(dimensions, request);
        }
    }

    private void setDimensions(ListContractResponse.Dimensions dimensions, PackingV3Request request) {
        if (dimensions.getLength() != null) {
            request.setLength(BigDecimal.valueOf(dimensions.getLength()));
        }
        if (dimensions.getWidth() != null) {
            request.setWidth(BigDecimal.valueOf(dimensions.getWidth()));
        }
        if (dimensions.getHeight() != null) {
            request.setHeight(BigDecimal.valueOf(dimensions.getHeight()));
        }
        request.setLengthUnit(dimensions.getUom());
        request.setHeightUnit(dimensions.getUom());
        request.setWidthUnit(dimensions.getUom());
        request.setIsDimension(true);
    }
}
