package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.impl.ProductSequenceConfigDao;
import com.dpw.runner.shipment.services.dao.interfaces.ITenantProductsDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.ProductSequenceConfig;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.TenantProducts;
import com.dpw.runner.shipment.services.entity.enums.ProductProcessTypes;
import com.dpw.runner.shipment.services.entity.enums.ProductType;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSettingsSync;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
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
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(CONCURRENT)
class ProductIdentifierUtilityTest {

    @InjectMocks
    private ProductIdentifierUtility productIdentifierUtility;

    @Mock
    private ITenantProductsDao tenantProductsDao;
    @Mock
    private ProductSequenceConfigDao productSequenceConfigDao;
    @Mock
    private GetNextNumberHelper getNextNumberHelper;
    @Mock
    private IShipmentSettingsSync shipmentSettingsSync;
    @Mock
    private V1AuthHelper v1AuthHelper;

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
        when(productSequenceConfigDao.findAndLock(any(Specification.class), any(Pageable.class))).thenReturn(productSequenceConfig);

        ListCommonRequest listRequest =
                CommonUtils.constructListCommonRequest("isCommonSequence", true, "=");
        String seqNumber = productIdentifierUtility.GetCommonSequenceNumber(Constants.TRANSPORT_MODE_SEA, ProductProcessTypes.HAWB);
        assertNotNull(seqNumber);
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
        when(productSequenceConfigDao.findAndLock(any(Specification.class), any(Pageable.class))).thenReturn(productSequenceConfig);

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
        when(productSequenceConfigDao.findAndLock(any(Specification.class), any(Pageable.class))).thenReturn(productSequenceConfig);

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
        when(productSequenceConfigDao.findAndLock(any(Specification.class), any(Pageable.class))).thenReturn(productSequenceConfig);

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
        when(productSequenceConfigDao.findAndLock(any(Specification.class), any(Pageable.class))).thenReturn(productSequenceConfig);

        ListCommonRequest listRequest =
                CommonUtils.constructListCommonRequest("isCommonSequence", true, "=");
        String seqNumber = productIdentifierUtility.GetCommonSequenceNumber(Constants.TRANSPORT_MODE_SEA, ProductProcessTypes.HAWB);
        assertNotNull(seqNumber);
    }

    @Test
    void getCommonSequenceNumberFormatNotContainsLTest() {
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
        productSequenceConfig.setPrefix("branchCode;1");
        productSequenceConfigList.add(productSequenceConfig);
        PageImpl<ProductSequenceConfig> productSequenceConfigPage = new PageImpl<>(productSequenceConfigList);

        when(tenantProductsDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(tenantProductsPage);
        when(productSequenceConfigDao.findAndLock(any(Specification.class), any(Pageable.class))).thenReturn(productSequenceConfig);

        ListCommonRequest listRequest =
                CommonUtils.constructListCommonRequest("isCommonSequence", true, "=");
        String seqNumber = productIdentifierUtility.GetCommonSequenceNumber(Constants.TRANSPORT_MODE_SEA, ProductProcessTypes.HAWB);
        assertNotNull(seqNumber);
    }

    @Test
    void getCommonSequenceNumberTenantCodeLTest() {
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
        productSequenceConfig.setPrefix("branchCode;L12345");
        productSequenceConfigList.add(productSequenceConfig);
        PageImpl<ProductSequenceConfig> productSequenceConfigPage = new PageImpl<>(productSequenceConfigList);

        when(tenantProductsDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(tenantProductsPage);
        when(productSequenceConfigDao.findAndLock(any(Specification.class), any(Pageable.class))).thenReturn(productSequenceConfig);

        ListCommonRequest listRequest =
                CommonUtils.constructListCommonRequest("isCommonSequence", true, "=");
        String seqNumber = productIdentifierUtility.GetCommonSequenceNumber(Constants.TRANSPORT_MODE_SEA, ProductProcessTypes.HAWB);
        assertNotNull(seqNumber);
    }

    @Test
    void getCommonSequenceNumberRegexSeqSerialCounterNullTest() {
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
        productSequenceConfig.setProductProcessTypes(ProductProcessTypes.HAWB);
        productSequenceConfig.setTenantProducts(tenantProduct);
        productSequenceConfig.setPrefix("seq;L1");
        productSequenceConfigList.add(productSequenceConfig);
        PageImpl<ProductSequenceConfig> productSequenceConfigPage = new PageImpl<>(productSequenceConfigList);

        when(tenantProductsDao.findAll(any(Specification.class), any(Pageable.class))).thenReturn(tenantProductsPage);
        when(productSequenceConfigDao.findAndLock(any(Specification.class), any(Pageable.class))).thenReturn(productSequenceConfig);

        ListCommonRequest listRequest =
                CommonUtils.constructListCommonRequest("isCommonSequence", true, "=");
        String seqNumber = productIdentifierUtility.GetCommonSequenceNumber(Constants.TRANSPORT_MODE_SEA, ProductProcessTypes.HAWB);
        assertNotNull(seqNumber);
    }

    @Test
    void getDefaultShipmentProductTest() {
        List<TenantProducts> tenantProductsList = new ArrayList<>();
        TenantProducts tenantProduct = new TenantProducts();
        tenantProduct.setTenantId(1);
        tenantProduct.setProductType(ProductType.Transport_All);
        tenantProduct.setTransportModes(Arrays.asList(Constants.TRANSPORT_MODE_SEA));
        tenantProductsList.add(tenantProduct);

        TenantProducts tenantProducts = productIdentifierUtility.getDefaultShipmentProduct(tenantProductsList);
        assertNotNull(tenantProducts);
    }
}
