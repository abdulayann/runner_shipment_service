package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.impl.ProductSequenceConfigDao;
import com.dpw.runner.shipment.services.dao.interfaces.ITenantProductsDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.ProductProcessTypes;
import com.dpw.runner.shipment.services.entity.enums.ProductType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.hibernate.validator.internal.util.Contracts.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class ProductIdentifierUtilityTest {

    @InjectMocks
    private ProductIdentifierUtility productIdentifierUtility;

    @Mock
    private ITenantProductsDao tenantProductsDao;
    @Mock
    private ProductSequenceConfigDao productSequenceConfigDao;
    @Mock
    private GetNextNumberHelper getNextNumberHelper;

    @BeforeAll
    static void init() throws IOException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setCode("code");
        UserContext.setUser(mockUser);
    }

    @Test
    void populateEnabledTenantProductsTest() {
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetails.builder().build();

        List<TenantProducts> tenantProductsList = new ArrayList<>();
        TenantProducts tenantProduct = new TenantProducts();
        tenantProduct.setTenantId(1);
        tenantProduct.setProductType(ProductType.Shipment_Air_IMP);
        tenantProductsList.add(tenantProduct);
        PageImpl<TenantProducts> tenantProductsPage = new PageImpl<>(tenantProductsList);

        when(tenantProductsDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(tenantProductsPage);
        List<TenantProducts> tenantProducts = productIdentifierUtility.populateEnabledTenantProducts(shipmentSettingsDetails);
        assertEquals(ProductType.Shipment_Air_IMP, tenantProducts.get(0).getProductType());
    }

    @Test
    void getCommonSequenceNumberTest() {
        List<TenantProducts> tenantProductsList = new ArrayList<>();
        TenantProducts tenantProduct = new TenantProducts();
        tenantProduct.setTenantId(1);
        tenantProduct.setProductType(ProductType.Shipment_Air_IMP);
        tenantProduct.setTransportModes(Arrays.asList(Constants.TRANSPORT_MODE_SEA));
        tenantProductsList.add(tenantProduct);
        PageImpl<TenantProducts> tenantProductsPage = new PageImpl<>(tenantProductsList);


        List<ProductSequenceConfig> productSequenceConfigList = new ArrayList<>();
        ProductSequenceConfig productSequenceConfig = new ProductSequenceConfig();
        productSequenceConfig.setTenantId(1);
        productSequenceConfig.setSerialCounter(1);
        productSequenceConfig.setProductProcessTypes(ProductProcessTypes.HAWB);
        productSequenceConfig.setTenantProducts(tenantProduct);
        productSequenceConfig.setPrefix("Aa");
        productSequenceConfigList.add(productSequenceConfig);
        PageImpl<ProductSequenceConfig> productSequenceConfigPage = new PageImpl<>(productSequenceConfigList);

        when(tenantProductsDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(tenantProductsPage);
        when(productSequenceConfigDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(productSequenceConfigPage);

        ListCommonRequest listRequest =
                CommonUtils.constructListCommonRequest("isCommonSequence", true, "=");
        String seqNumber = productIdentifierUtility.GetCommonSequenceNumber(Constants.TRANSPORT_MODE_SEA, ProductProcessTypes.HAWB);
        assertNotNull(seqNumber);
    }

    @Test
    void getShipmentProductWithOutContainerType() {
        List<TenantProducts> tenantProductsList = new ArrayList<>();
        TenantProducts tenantProduct = new TenantProducts();
        tenantProduct.setTenantId(1);
        tenantProduct.setProductType(ProductType.Shipment_Air_IMP);
        tenantProduct.setTransportModes(Arrays.asList(Constants.TRANSPORT_MODE_SEA));
        tenantProductsList.add(tenantProduct);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);

        ProductSequenceConfig result = new ProductSequenceConfig();
        result.setProductProcessTypes(ProductProcessTypes.HAWB);

        ProductSequenceConfig productSequenceConfig = productIdentifierUtility.getShipmentProductWithOutContainerType(shipmentDetails, ProductProcessTypes.HAWB, tenantProductsList);
        assertEquals(null, productSequenceConfig);
    }

    @Test
    void getShipmentProductWithOutContainerTypeAirImp() {
        List<TenantProducts> tenantProductsList = new ArrayList<>();
        TenantProducts tenantProduct = new TenantProducts();
        tenantProduct.setTenantId(1);
        tenantProduct.setProductType(ProductType.Shipment_Air_IMP);
        tenantProduct.setTransportModes(Arrays.asList(Constants.TRANSPORT_MODE_SEA));
        tenantProductsList.add(tenantProduct);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setDirection(Constants.DIRECTION_IMP);
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);

        ProductSequenceConfig result = new ProductSequenceConfig();
        result.setProductProcessTypes(ProductProcessTypes.HAWB);

        ProductSequenceConfig productSequenceConfig = productIdentifierUtility.getShipmentProductWithOutContainerType(shipmentDetails, ProductProcessTypes.HAWB, tenantProductsList);
        assertEquals(null, productSequenceConfig);
    }

    @Test
    void getShipmentProductWithOutContainerTypeAirExp() {
        List<TenantProducts> tenantProductsList = new ArrayList<>();
        TenantProducts tenantProduct = new TenantProducts();
        tenantProduct.setTenantId(1);
        tenantProduct.setProductType(ProductType.Shipment_Air_EXP);
        tenantProduct.setTransportModes(Arrays.asList(Constants.TRANSPORT_MODE_AIR));
        tenantProductsList.add(tenantProduct);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);

        ProductSequenceConfig result = new ProductSequenceConfig();
        result.setProductProcessTypes(ProductProcessTypes.HAWB);

        ProductSequenceConfig productSequenceConfig = productIdentifierUtility.getShipmentProductWithOutContainerType(shipmentDetails, ProductProcessTypes.HAWB, tenantProductsList);
        assertEquals(null, productSequenceConfig);
    }

    @Test
    void getShipmentProductWithOutContainerTypeAirCrossTrade() {
        List<TenantProducts> tenantProductsList = new ArrayList<>();
        TenantProducts tenantProduct = new TenantProducts();
        tenantProduct.setTenantId(1);
        tenantProduct.setProductType(ProductType.Shipment_Air_CrossTrade);
        tenantProduct.setTransportModes(Arrays.asList(Constants.TRANSPORT_MODE_AIR));
        tenantProductsList.add(tenantProduct);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setDirection("CTS");
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);

        ProductSequenceConfig result = new ProductSequenceConfig();
        result.setProductProcessTypes(ProductProcessTypes.HAWB);

        ProductSequenceConfig productSequenceConfig = productIdentifierUtility.getShipmentProductWithOutContainerType(shipmentDetails, ProductProcessTypes.HAWB, tenantProductsList);
        assertEquals(null, productSequenceConfig);
    }

    @Test
    void getShipmentProductWithOutContainerTypeAirTransShip() {
        List<TenantProducts> tenantProductsList = new ArrayList<>();
        TenantProducts tenantProduct = new TenantProducts();
        tenantProduct.setTenantId(1);
        tenantProduct.setProductType(ProductType.Shipment_Air_TransShip);
        tenantProduct.setTransportModes(Arrays.asList(Constants.TRANSPORT_MODE_AIR));
        tenantProductsList.add(tenantProduct);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setDirection("TRA");
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);

        ProductSequenceConfig result = new ProductSequenceConfig();
        result.setProductProcessTypes(ProductProcessTypes.HAWB);

        ProductSequenceConfig productSequenceConfig = productIdentifierUtility.getShipmentProductWithOutContainerType(shipmentDetails, ProductProcessTypes.HAWB, tenantProductsList);
        assertEquals(null, productSequenceConfig);
    }

    @Test
    void getShipmentProductWithOutContainerTypeAir() {
        List<TenantProducts> tenantProductsList = new ArrayList<>();
        TenantProducts tenantProduct = new TenantProducts();
        tenantProduct.setTenantId(1);
        tenantProduct.setProductType(ProductType.Shipment_Air_CrossTrade);
        tenantProduct.setTransportModes(Arrays.asList(Constants.TRANSPORT_MODE_AIR));
        tenantProductsList.add(tenantProduct);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setDirection("IMP");
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);

        ProductSequenceConfig result = new ProductSequenceConfig();
        result.setProductProcessTypes(ProductProcessTypes.HAWB);

        ProductSequenceConfig productSequenceConfig = productIdentifierUtility.getShipmentProductWithOutContainerType(shipmentDetails, ProductProcessTypes.HAWB, tenantProductsList);
        assertEquals(null, productSequenceConfig);
    }

    @Test
    void getShipmentProductWithOutContainerTypeSeaImp() {
        List<TenantProducts> tenantProductsList = new ArrayList<>();
        TenantProducts tenantProduct = new TenantProducts();
        tenantProduct.setTenantId(1);
        tenantProduct.setProductType(ProductType.Shipment_Sea_IMP);
        tenantProduct.setTransportModes(Arrays.asList(Constants.TRANSPORT_MODE_SEA));
        tenantProductsList.add(tenantProduct);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setDirection("IMP");
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);

        ProductSequenceConfig result = new ProductSequenceConfig();
        result.setProductProcessTypes(ProductProcessTypes.HAWB);

        ProductSequenceConfig productSequenceConfig = productIdentifierUtility.getShipmentProductWithOutContainerType(shipmentDetails, ProductProcessTypes.HAWB, tenantProductsList);
        assertEquals(null, productSequenceConfig);
    }

    @Test
    void getShipmentProductWithOutContainerTypeSeaExp() {
        List<TenantProducts> tenantProductsList = new ArrayList<>();
        TenantProducts tenantProduct = new TenantProducts();
        tenantProduct.setTenantId(1);
        tenantProduct.setProductType(ProductType.Shipment_Sea_EXP);
        tenantProduct.setTransportModes(Arrays.asList(Constants.TRANSPORT_MODE_SEA));
        tenantProductsList.add(tenantProduct);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setDirection("EXP");
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);

        ProductSequenceConfig result = new ProductSequenceConfig();
        result.setProductProcessTypes(ProductProcessTypes.HAWB);

        ProductSequenceConfig productSequenceConfig = productIdentifierUtility.getShipmentProductWithOutContainerType(shipmentDetails, ProductProcessTypes.HAWB, tenantProductsList);
        assertEquals(null, productSequenceConfig);
    }

    @Test
    void getShipmentProductWithOutContainerTypeSeaCrossTrade() {
        List<TenantProducts> tenantProductsList = new ArrayList<>();
        TenantProducts tenantProduct = new TenantProducts();
        tenantProduct.setTenantId(1);
        tenantProduct.setProductType(ProductType.Shipment_Sea_CrossTrade);
        tenantProduct.setTransportModes(Arrays.asList(Constants.TRANSPORT_MODE_SEA));
        tenantProductsList.add(tenantProduct);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setDirection("CTS");
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);

        ProductSequenceConfig result = new ProductSequenceConfig();
        result.setProductProcessTypes(ProductProcessTypes.HAWB);

        ProductSequenceConfig productSequenceConfig = productIdentifierUtility.getShipmentProductWithOutContainerType(shipmentDetails, ProductProcessTypes.HAWB, tenantProductsList);
        assertEquals(null, productSequenceConfig);
    }

    @Test
    void getShipmentProductWithOutContainerTypeSeaTrans() {
        List<TenantProducts> tenantProductsList = new ArrayList<>();
        TenantProducts tenantProduct = new TenantProducts();
        tenantProduct.setTenantId(1);
        tenantProduct.setProductType(ProductType.Shipment_Sea_TransShip);
        tenantProduct.setTransportModes(Arrays.asList(Constants.TRANSPORT_MODE_SEA));
        tenantProductsList.add(tenantProduct);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setDirection("TRA");
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);

        ProductSequenceConfig result = new ProductSequenceConfig();
        result.setProductProcessTypes(ProductProcessTypes.HAWB);

        ProductSequenceConfig productSequenceConfig = productIdentifierUtility.getShipmentProductWithOutContainerType(shipmentDetails, ProductProcessTypes.HAWB, tenantProductsList);
        assertEquals(null, productSequenceConfig);
    }

    @Test
    void getShipmentProductWithOutContainerTypeSea() {
        List<TenantProducts> tenantProductsList = new ArrayList<>();
        TenantProducts tenantProduct = new TenantProducts();
        tenantProduct.setTenantId(1);
        tenantProduct.setTransportModes(Arrays.asList(Constants.TRANSPORT_MODE_SEA));
        tenantProductsList.add(tenantProduct);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setDirection("TRA");
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);

        ProductSequenceConfig result = new ProductSequenceConfig();
        result.setProductProcessTypes(ProductProcessTypes.HAWB);

        ProductSequenceConfig productSequenceConfig = productIdentifierUtility.getShipmentProductWithOutContainerType(shipmentDetails, ProductProcessTypes.HAWB, tenantProductsList);
        assertEquals(null, productSequenceConfig);
    }

    @Test
    void getShipmentProductWithOutContainerTypeRoadExp() {
        List<TenantProducts> tenantProductsList = new ArrayList<>();
        TenantProducts tenantProduct = new TenantProducts();
        tenantProduct.setTenantId(1);
        tenantProduct.setProductType(ProductType.Shipment_Road_EXP);
        tenantProduct.setTransportModes(Arrays.asList(Constants.TRANSPORT_MODE_ROA));
        tenantProductsList.add(tenantProduct);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_ROA);
        shipmentDetails.setDirection("EXP");
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);

        ProductSequenceConfig result = new ProductSequenceConfig();
        result.setProductProcessTypes(ProductProcessTypes.HAWB);

        ProductSequenceConfig productSequenceConfig = productIdentifierUtility.getShipmentProductWithOutContainerType(shipmentDetails, ProductProcessTypes.HAWB, tenantProductsList);
        assertEquals(null, productSequenceConfig);
    }

    @Test
    void getShipmentProductWithOutContainerTypeRoadCrossTrade() {
        List<TenantProducts> tenantProductsList = new ArrayList<>();
        TenantProducts tenantProduct = new TenantProducts();
        tenantProduct.setTenantId(1);
        tenantProduct.setProductType(ProductType.Shipment_Road_CrossTrade);
        tenantProduct.setTransportModes(Arrays.asList(Constants.TRANSPORT_MODE_ROA));
        tenantProductsList.add(tenantProduct);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_ROA);
        shipmentDetails.setDirection("CTS");
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);

        ProductSequenceConfig result = new ProductSequenceConfig();
        result.setProductProcessTypes(ProductProcessTypes.HAWB);

        ProductSequenceConfig productSequenceConfig = productIdentifierUtility.getShipmentProductWithOutContainerType(shipmentDetails, ProductProcessTypes.HAWB, tenantProductsList);
        assertEquals(null, productSequenceConfig);
    }

    @Test
    void getShipmentProductWithOutContainerTypeRoadTrans() {
        List<TenantProducts> tenantProductsList = new ArrayList<>();
        TenantProducts tenantProduct = new TenantProducts();
        tenantProduct.setTenantId(1);
        tenantProduct.setProductType(ProductType.Shipment_Road_TransShip);
        tenantProduct.setTransportModes(Arrays.asList(Constants.TRANSPORT_MODE_ROA));
        tenantProductsList.add(tenantProduct);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_ROA);
        shipmentDetails.setDirection("TRA");
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);

        ProductSequenceConfig result = new ProductSequenceConfig();
        result.setProductProcessTypes(ProductProcessTypes.HAWB);

        ProductSequenceConfig productSequenceConfig = productIdentifierUtility.getShipmentProductWithOutContainerType(shipmentDetails, ProductProcessTypes.HAWB, tenantProductsList);
        assertEquals(null, productSequenceConfig);
    }

    @Test
    void getShipmentProductWithOutContainerTypeRailCross() {
        List<TenantProducts> tenantProductsList = new ArrayList<>();
        TenantProducts tenantProduct = new TenantProducts();
        tenantProduct.setTenantId(1);
        tenantProduct.setProductType(ProductType.Shipment_Rail_CrossTrade);
        tenantProduct.setTransportModes(Arrays.asList(Constants.TRANSPORT_MODE_RAI));
        tenantProductsList.add(tenantProduct);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_RAI);
        shipmentDetails.setDirection("CTS");
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);

        ProductSequenceConfig result = new ProductSequenceConfig();
        result.setProductProcessTypes(ProductProcessTypes.HAWB);

        ProductSequenceConfig productSequenceConfig = productIdentifierUtility.getShipmentProductWithOutContainerType(shipmentDetails, ProductProcessTypes.HAWB, tenantProductsList);
        assertEquals(null, productSequenceConfig);
    }

    @Test
    void getShipmentProductWithOutContainerTypeRailTrans() {
        List<TenantProducts> tenantProductsList = new ArrayList<>();
        TenantProducts tenantProduct = new TenantProducts();
        tenantProduct.setTenantId(1);
        tenantProduct.setProductType(ProductType.Shipment_Rail_TransShip);
        tenantProduct.setTransportModes(Arrays.asList(Constants.TRANSPORT_MODE_RAI));
        tenantProductsList.add(tenantProduct);

        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_RAI);
        shipmentDetails.setDirection("TRA");
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);

        ProductSequenceConfig result = new ProductSequenceConfig();
        result.setProductProcessTypes(ProductProcessTypes.HAWB);

        ProductSequenceConfig productSequenceConfig = productIdentifierUtility.getShipmentProductWithOutContainerType(shipmentDetails, ProductProcessTypes.HAWB, tenantProductsList);
        assertEquals(null, productSequenceConfig);
    }

    @Test
    void getCustomizedBLNumberTest() throws RunnerException {
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetails.builder().build();
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);

        List<TenantProducts> tenantProductsList = new ArrayList<>();
        TenantProducts tenantProduct = new TenantProducts();
        tenantProduct.setTenantId(1);
        tenantProduct.setProductType(ProductType.Shipment_Air_IMP);
        tenantProduct.setTransportModes(Arrays.asList(Constants.TRANSPORT_MODE_SEA));
        tenantProductsList.add(tenantProduct);
        PageImpl<TenantProducts> tenantProductsPage = new PageImpl<>(tenantProductsList);

        List<ProductSequenceConfig> productSequenceConfigList = new ArrayList<>();
        ProductSequenceConfig productSequenceConfig = new ProductSequenceConfig();
        productSequenceConfig.setTenantId(1);
        productSequenceConfig.setSerialCounter(1);
        productSequenceConfig.setProductProcessTypes(ProductProcessTypes.HBLNumber);
        productSequenceConfig.setTenantProducts(tenantProduct);
        productSequenceConfig.setPrefix("Aa");
        productSequenceConfigList.add(productSequenceConfig);
        PageImpl<ProductSequenceConfig> productSequenceConfigPage = new PageImpl<>(productSequenceConfigList);

        when(tenantProductsDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(tenantProductsPage);
        when(productSequenceConfigDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(productSequenceConfigPage);

        String number = productIdentifierUtility.getCustomizedBLNumber(shipmentDetails, shipmentSettingsDetails);
        assertNotNull(number);
    }

    @Test
    void IdentifyProduct() {
        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);

        List<TenantProducts> tenantProductsList = new ArrayList<>();
        TenantProducts tenantProduct = new TenantProducts();
        tenantProduct.setTenantId(1);
        tenantProduct.setProductType(ProductType.Consolidation_Sea_EXIM);
        tenantProduct.setTransportModes(Arrays.asList(Constants.TRANSPORT_MODE_SEA));
        tenantProductsList.add(tenantProduct);

        TenantProducts tenantProducts = productIdentifierUtility.IdentifyProduct(consolidationDetails, tenantProductsList);
        assertNotNull(tenantProducts);
    }

    @Test
    void IdentifyProductAirConsol() {
        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);

        List<TenantProducts> tenantProductsList = new ArrayList<>();
        TenantProducts tenantProduct = new TenantProducts();
        tenantProduct.setTenantId(1);
        tenantProduct.setProductType(ProductType.Consolidation_Air_EXIM);
        tenantProduct.setTransportModes(Arrays.asList(Constants.TRANSPORT_MODE_AIR));
        tenantProductsList.add(tenantProduct);

        TenantProducts tenantProducts = productIdentifierUtility.IdentifyProduct(consolidationDetails, tenantProductsList);
        assertNotNull(tenantProducts);
    }

    @Test
    void IdentifyProductConsolidation_All() {
        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);

        List<TenantProducts> tenantProductsList = new ArrayList<>();
        TenantProducts tenantProduct = new TenantProducts();
        tenantProduct.setTenantId(1);
        tenantProduct.setProductType(ProductType.Consolidation_All);
        tenantProduct.setTransportModes(Arrays.asList(Constants.TRANSPORT_MODE_AIR));
        tenantProductsList.add(tenantProduct);

        TenantProducts tenantProducts = productIdentifierUtility.IdentifyProduct(consolidationDetails, tenantProductsList);
        assertNotNull(tenantProducts);
    }

    @Test
    void getCommonSequenceNumberRegexTest() {
        List<TenantProducts> tenantProductsList = new ArrayList<>();
        TenantProducts tenantProduct = new TenantProducts();
        tenantProduct.setTenantId(1);
        tenantProduct.setProductType(ProductType.Shipment_Air_IMP);
        tenantProduct.setTransportModes(Arrays.asList(Constants.TRANSPORT_MODE_SEA));
        tenantProductsList.add(tenantProduct);
        PageImpl<TenantProducts> tenantProductsPage = new PageImpl<>(tenantProductsList);


        List<ProductSequenceConfig> productSequenceConfigList = new ArrayList<>();
        ProductSequenceConfig productSequenceConfig = new ProductSequenceConfig();
        productSequenceConfig.setTenantId(1);
        productSequenceConfig.setSerialCounter(1);
        productSequenceConfig.setProductProcessTypes(ProductProcessTypes.HAWB);
        productSequenceConfig.setTenantProducts(tenantProduct);
        productSequenceConfig.setPrefix("branchCode;L1");
        productSequenceConfigList.add(productSequenceConfig);
        PageImpl<ProductSequenceConfig> productSequenceConfigPage = new PageImpl<>(productSequenceConfigList);

        when(tenantProductsDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(tenantProductsPage);
        when(productSequenceConfigDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(productSequenceConfigPage);

        ListCommonRequest listRequest =
                CommonUtils.constructListCommonRequest("isCommonSequence", true, "=");
        String seqNumber = productIdentifierUtility.GetCommonSequenceNumber(Constants.TRANSPORT_MODE_SEA, ProductProcessTypes.HAWB);
        assertNotNull(seqNumber);
    }

    @Test
    void getCommonSequenceNumberRegexTransportModeTest() {
        List<TenantProducts> tenantProductsList = new ArrayList<>();
        TenantProducts tenantProduct = new TenantProducts();
        tenantProduct.setTenantId(1);
        tenantProduct.setProductType(ProductType.Shipment_Air_IMP);
        tenantProduct.setTransportModes(Arrays.asList(Constants.TRANSPORT_MODE_SEA));
        tenantProductsList.add(tenantProduct);
        PageImpl<TenantProducts> tenantProductsPage = new PageImpl<>(tenantProductsList);


        List<ProductSequenceConfig> productSequenceConfigList = new ArrayList<>();
        ProductSequenceConfig productSequenceConfig = new ProductSequenceConfig();
        productSequenceConfig.setTenantId(1);
        productSequenceConfig.setSerialCounter(1);
        productSequenceConfig.setProductProcessTypes(ProductProcessTypes.HAWB);
        productSequenceConfig.setTenantProducts(tenantProduct);
        productSequenceConfig.setPrefix("transportmode;1");
        productSequenceConfigList.add(productSequenceConfig);
        PageImpl<ProductSequenceConfig> productSequenceConfigPage = new PageImpl<>(productSequenceConfigList);

        when(tenantProductsDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(tenantProductsPage);
        when(productSequenceConfigDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(productSequenceConfigPage);

        ListCommonRequest listRequest =
                CommonUtils.constructListCommonRequest("isCommonSequence", true, "=");
        String seqNumber = productIdentifierUtility.GetCommonSequenceNumber(Constants.TRANSPORT_MODE_SEA, ProductProcessTypes.HAWB);
        assertNotNull(seqNumber);
    }

    @Test
    void getCommonSequenceNumberRegexDateTest() {
        List<TenantProducts> tenantProductsList = new ArrayList<>();
        TenantProducts tenantProduct = new TenantProducts();
        tenantProduct.setTenantId(1);
        tenantProduct.setProductType(ProductType.Shipment_Air_IMP);
        tenantProduct.setTransportModes(Arrays.asList(Constants.TRANSPORT_MODE_SEA));
        tenantProductsList.add(tenantProduct);
        PageImpl<TenantProducts> tenantProductsPage = new PageImpl<>(tenantProductsList);


        List<ProductSequenceConfig> productSequenceConfigList = new ArrayList<>();
        ProductSequenceConfig productSequenceConfig = new ProductSequenceConfig();
        productSequenceConfig.setTenantId(1);
        productSequenceConfig.setSerialCounter(1);
        productSequenceConfig.setProductProcessTypes(ProductProcessTypes.HAWB);
        productSequenceConfig.setTenantProducts(tenantProduct);
        productSequenceConfig.setPrefix("date;L1");
        productSequenceConfigList.add(productSequenceConfig);
        PageImpl<ProductSequenceConfig> productSequenceConfigPage = new PageImpl<>(productSequenceConfigList);

        when(tenantProductsDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(tenantProductsPage);
        when(productSequenceConfigDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(productSequenceConfigPage);

        ListCommonRequest listRequest =
                CommonUtils.constructListCommonRequest("isCommonSequence", true, "=");
        String seqNumber = productIdentifierUtility.GetCommonSequenceNumber(Constants.TRANSPORT_MODE_SEA, ProductProcessTypes.HAWB);
        assertNotNull(seqNumber);
    }

    @Test
    void getCommonSequenceNumberRegexSeqTest() {
        List<TenantProducts> tenantProductsList = new ArrayList<>();
        TenantProducts tenantProduct = new TenantProducts();
        tenantProduct.setTenantId(1);
        tenantProduct.setProductType(ProductType.Shipment_Air_IMP);
        tenantProduct.setTransportModes(Arrays.asList(Constants.TRANSPORT_MODE_SEA));
        tenantProductsList.add(tenantProduct);
        PageImpl<TenantProducts> tenantProductsPage = new PageImpl<>(tenantProductsList);


        List<ProductSequenceConfig> productSequenceConfigList = new ArrayList<>();
        ProductSequenceConfig productSequenceConfig = new ProductSequenceConfig();
        productSequenceConfig.setTenantId(1);
        productSequenceConfig.setSerialCounter(1);
        productSequenceConfig.setProductProcessTypes(ProductProcessTypes.HAWB);
        productSequenceConfig.setTenantProducts(tenantProduct);
        productSequenceConfig.setPrefix("seq;L1");
        productSequenceConfigList.add(productSequenceConfig);
        PageImpl<ProductSequenceConfig> productSequenceConfigPage = new PageImpl<>(productSequenceConfigList);

        when(tenantProductsDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(tenantProductsPage);
        when(productSequenceConfigDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(productSequenceConfigPage);

        ListCommonRequest listRequest =
                CommonUtils.constructListCommonRequest("isCommonSequence", true, "=");
        String seqNumber = productIdentifierUtility.GetCommonSequenceNumber(Constants.TRANSPORT_MODE_SEA, ProductProcessTypes.HAWB);
        assertNotNull(seqNumber);
    }
}
