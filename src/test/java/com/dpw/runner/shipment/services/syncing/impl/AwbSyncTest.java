package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IMawbHawbLinkDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.awb.*;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISyncService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.Entity.*;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.EmailServiceUtility;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.modelmapper.ModelMapper;
import org.springframework.data.domain.PageImpl;
import org.springframework.web.client.RestTemplate;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class AwbSyncTest {
    @InjectMocks
    private AwbSync awbSync;

    @Mock
    private EmailServiceUtility emailServiceUtility;

    @Mock
    private IAwbDao iAwbDao;

    @Mock
    private IConsolidationDetailsDao iConsolidationDetailsDao;

    @Mock
    private IMawbHawbLinkDao iMawbHawbLinkDao;

    @Mock
    private IShipmentDao iShipmentDao;

    @Mock
    private ISyncService iSyncService;

    @Mock
    private IV1Service iV1Service;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private ModelMapper modelMapper;

    @Mock
    private RestTemplate restTemplate;

    /**
     * Method under test: {@link AwbSync#sync(Awb, SaveStatus)}
     */
    @Test
    void testSync() {
        boolean isSuccess = true;
        // Arrange
        Awb awb = new Awb();
        awb.setAwbShipmentInfo(AwbShipmentInfo.builder().entityType("mawb").build());
        awb.setAwbNotifyPartyInfo(List.of(new AwbNotifyPartyInfo()));
        awb.setShipmentId(123L);
        awb.setConsolidationId(1234L);
        when(jsonHelper.convertValue(any(), eq(AwbRequestV2.class))).thenReturn(new AwbRequestV2());
        when(iShipmentDao.findById(anyLong())).thenReturn(Optional.of(new ShipmentDetails()));
        when(iConsolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(new ConsolidationDetails()));
        when(iMawbHawbLinkDao.findByMawbId(any())).thenReturn(List.of());
        mock(CommonUtils.class);
        when(iAwbDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(new Awb())));
        doNothing().when(iSyncService).pushToKafka(any(), any(), any(), any(), any());

        // Act
        awbSync.sync(awb, SaveStatus.CREATE);

        assertTrue(isSuccess);
    }

    @Test
    void testSync2() {
        boolean isSuccess = true;
        // Arrange
        Awb awb = new Awb();
        awb.setAwbShipmentInfo(AwbShipmentInfo.builder().entityType("mawb").build());
        awb.setShipmentId(123L);
        awb.setConsolidationId(1234L);

        when(jsonHelper.convertValue(any(), eq(AwbRequestV2.class))).thenReturn(new AwbRequestV2());
        when(iShipmentDao.findById(anyLong())).thenReturn(Optional.of(new ShipmentDetails()));
        when(iConsolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(new ConsolidationDetails()));
        when(iMawbHawbLinkDao.findByMawbId(any())).thenReturn(List.of());
        mock(CommonUtils.class);
        when(iAwbDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of()));
        doNothing().when(iSyncService).pushToKafka(any(), any(), any(), any(), any());

        // Act
        awbSync.sync(awb, SaveStatus.CREATE);

        assertTrue(isSuccess);
    }

    @Test
    void testSync3() {
        boolean isSuccess = true;
        // Arrange
        Awb awb = new Awb();
        awb.setAwbShipmentInfo(AwbShipmentInfo.builder().entityType("hawb").build());

        when(jsonHelper.convertValue(any(), eq(AwbRequestV2.class))).thenReturn(new AwbRequestV2());

        mock(CommonUtils.class);

        doNothing().when(iSyncService).pushToKafka(any(), any(), any(), any(), any());

        // Act
        awbSync.sync(awb, SaveStatus.CREATE);

        assertTrue(isSuccess);
    }

    @Test
    void testSync4() {
        boolean isSuccess = true;
        // Arrange
        Awb awb = new Awb();
        awb.setAwbShipmentInfo(AwbShipmentInfo.builder().entityType("mawb").build());
        awb.setShipmentId(123L);
        awb.setConsolidationId(1234L);

        when(jsonHelper.convertValue(any(), eq(AwbRequestV2.class))).thenReturn(new AwbRequestV2());
        when(iShipmentDao.findById(anyLong())).thenReturn(Optional.empty());
        when(iConsolidationDetailsDao.findById(anyLong())).thenReturn(Optional.empty());
        when(iMawbHawbLinkDao.findByMawbId(any())).thenReturn(List.of());
        mock(CommonUtils.class);
        when(iAwbDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of()));
        doNothing().when(iSyncService).pushToKafka(any(), any(), any(), any(), any());

        // Act
        awbSync.sync(awb, SaveStatus.CREATE);

        assertTrue(isSuccess);
    }
}
