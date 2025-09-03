package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.dto.response.ListContractResponse;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class NpmContractV3UtilTest {

    @Mock
    private MasterDataUtils masterDataUtils;

    @InjectMocks
    private NpmContractV3Util npmContractV3Util;

    @Test
    void testCreateCarrierDetails_withValidData() {
        var meta = ListContractResponse.Meta.builder()
                .pol("INMAA")
                .pod("USNYC")
                .minTransitHours("100")
                .maxTransitHours("200")
                .build();

        var contract = ListContractResponse.ContractResponse.builder()
                .origin("IN")
                .destination("US")
                .carrier_codes(List.of("MAEU"))
                .meta(meta)
                .build();

        var carrier = new EntityTransferCarrier();
        carrier.setItemValue("Maersk");

        when(masterDataUtils.fetchInBulkCarriersBySCACCode(List.of("MAEU")))
                .thenReturn(Map.of("MAEU", carrier));

        var details = npmContractV3Util.createCarrierDetails(contract);

        assertEquals("IN", details.getOrigin());
        assertEquals("US", details.getDestination());
        assertEquals("INMAA", details.getOriginPort());
        assertEquals("USNYC", details.getDestinationPort());
        assertEquals("100", details.getMinTransitHours());
        assertEquals("200", details.getMaxTransitHours());
        assertEquals("Maersk", details.getShippingLine());
    }

    @Test
    void testSetFilterParams() {
        var filters = ListContractResponse.FilterParams.builder()
                .cargo_type(List.of("GENERAL"))
                .commodity(List.of("FOOD"))
                .build();

        var request = new PackingV3Request();
        npmContractV3Util.setFilterParams(filters, request);

        assertEquals("GENERAL", request.getPacksType());
        assertEquals("FOOD", request.getCommodityGroup());
    }

    @Test
    void testSetMetaData_withDimensions() {
        var dimensions = ListContractResponse.Dimensions.builder()
                .length(10L).width(20L).height(30L).uom("CM")
                .build();

        var loadAttributes = ListContractResponse.LoadAttributes.builder()
                .quantity(5L)
                .weight(BigDecimal.valueOf(100))
                .weight_uom("KG")
                .volume(BigDecimal.valueOf(10))
                .volume_uom("CBM")
                .dimensions(dimensions)
                .build();

        var meta = ListContractResponse.ContractUsageMeta.builder()
                .load_attributes(loadAttributes)
                .build();

        var usage = ListContractResponse.ContractUsage.builder()
                .meta(meta)
                .build();

        var request = new PackingV3Request();
        npmContractV3Util.setMetaData(usage, request);

        assertEquals("5", request.getPacks());
        assertEquals(BigDecimal.valueOf(100), request.getWeight());
        assertEquals("KG", request.getWeightUnit());
        assertEquals(BigDecimal.valueOf(10), request.getVolume());
        assertEquals("CBM", request.getVolumeUnit());
        assertTrue(request.getIsDimension());
        assertEquals(BigDecimal.valueOf(10), request.getLength());
        assertEquals(BigDecimal.valueOf(20), request.getWidth());
        assertEquals(BigDecimal.valueOf(30), request.getHeight());
        assertEquals("CM", request.getLengthUnit());
    }
}