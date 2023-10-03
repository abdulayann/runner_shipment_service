//package com.dpw.runner.shipment.services.ReportingService.Reports;
//
//import com.dpw.runner.shipment.services.ReportingService.Models.HawbModel;
//import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
//import com.dpw.runner.shipment.services.ReportingService.Models.PreAlertModel;
//import com.dpw.runner.shipment.services.helpers.JsonHelper;
//import org.springframework.beans.factory.annotation.Autowired;
//
//import java.util.Map;
//
//public class PreAlertReport implements IReport{
//
//    @Autowired
//    private JsonHelper jsonHelper;
//
//    @Override
//    public Map<String, Object> getData(Long id) {
//    }
//
//    @Override
//    IDocumentModel getDocumentModel(Long id) {
//        return null;
//    }
//
//    @Override
//    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
//        PreAlertModel qc_data = (PreAlertModel) documentModel;
//        String json = jsonHelper.convertToJson(preAlertModel.shipment);
//        Map<String, Object> dictionary = jsonHelper.convertJsonToMap(json);
//        var user = (UserDefinition)Authorization.UserDefinition;
//
//        AddressRow deliveryAgentAddress = null;
//        UnLocationsRow originDetail = null;
//        UnLocationsRow destinationDetail = null;
//        ConsolidationRow consolRow = null;
//        List<PackingRow> packsDetails = null;
//
//        AddressRepository addressRepository = new AddressRepository();
//        UnLocationsRepository unlocationRepository = new UnLocationsRepository();
//        List<CommonContainersRow> allContainersList = new List<CommonContainersRow>();
//
//        using (var connection = SqlConnections.NewFor<ShipmentsRow>())
//        {
//            var o = ShipmentsRow.Fields;
//            var IncludeColumns = new List<string>
//            {
//
//            };
//            var includehashset = new HashSet<string>(IncludeColumns);
//            var retreq = new RetrieveRequest
//            {
//                IncludeColumns = includehashset,
//                        ColumnSelection = RetrieveColumnSelection.Details,
//            };
//
//            retreq.EntityId = this.ShipmentId;
//
//            var repo = new ShipmentsRepository();
//            var res = repo.Retrieve(connection, retreq);
//            qc_data.shipment = res.Entity;
//
//
//            var crepo = new TenantsRepository();
//            retreq.EntityId = user.TenantId;
//            var cres = crepo.Retrieve(connection, retreq);
//            qc_data.shipment = res.Entity;
//
//            consolRow = getFirstConsolidation(user.TenantId, connection);
//
//            qc_data.tenant = cres.Entity;
//            if (qc_data.shipment.ShipmentContainers == null || qc_data.shipment.ShipmentContainers.Count == 0)
//            {
//                List<ShipmentContainersRow> list = new List<ShipmentContainersRow>();
//
//                // foreach (Int64 Id in qc_data.shipment.ConsolidationIds)
//                // {
//                var fld = CommonContainersRow.Fields;
//                var repository = new CommonContainersRepository();
//
//                var row = new CommonContainersRow();
//                var listrequest = new ListRequest
//                {
//                    Criteria = (new Criteria("ShipmentId") == qc_data.shipment.Id.ToString() & new Criteria("EntityType").In("[Shipments]", "[Consolidations]"))
//                };
//
//                using (var uow = new UnitOfWork(connection))
//                {
//                    var criteria = new Criteria("EntityType").In("[Shipments]", "[Consolidations]");
//                    var lists = repository.CustomList(uow.Connection, listrequest,  criteria, qc_data.shipment.Id).Entities;
//                    allContainersList = lists;
//                    if (lists != null)
//                    {
//                        foreach (var item in lists)
//                        {
//                            list.Add(getShipmentContainer(item));
//                        }
//
//                    }
//                }
//                // }
//                qc_data.shipment.ShipmentContainers = list;
//            }
//
//            var packingfld = PackingRow.Fields;
//            var packsRepo = new PackingRepository();
//            var listreq = new ListRequest
//            {
//                Criteria = (new Criteria("ShipmentId") == this.ShipmentId)
//            };
//            try
//            {
//                using (var uow = new UnitOfWork(connection))
//                {
//                    packsDetails = packsRepo.List(uow.Connection, listreq).Entities;
//                }
//            }
//            catch (Exception)
//            {
//                packsDetails = null;
//            }
//
//            if(qc_data.shipment.DeliveryAgentAddressId != null)
//            {
//                retreq.EntityId = qc_data.shipment.DeliveryAgentAddressId;
//                var res1 = addressRepository.Retrieve(connection, retreq);
//                deliveryAgentAddress = res1.Entity;
//            }
//
//            if(qc_data.shipment.Origin != null)
//            {
//                retreq.EntityId = qc_data.shipment.Origin;
//                var res2 = unlocationRepository.Retrieve(connection, retreq);
//                originDetail = res2.Entity;
//            }
//
//
//            if(qc_data.shipment.Destination != null)
//            {
//                retreq.EntityId = qc_data.shipment.Destination;
//                var res3 = unlocationRepository.Retrieve(connection, retreq);
//                destinationDetail = res3.Entity;
//            }
//        }
//
//        qc_data.noofpackages_word = GetNextNumberHelper.NumberToWords(qc_data.shipment.Packs);
//        qc_data.userdisplayname = user.DisplayName;
//
//        var serializeQuote = JsonConvert.SerializeObject(qc_data.shipment);
//        var dictionary = JsonConvert.DeserializeObject<Dictionary<string, object>>(serializeQuote);
//        PrintUtil.JsonDateFormat(ref dictionary);
//        ReportHelper.addTenantDetails(dictionary, qc_data.tenant);
//
//        var consigner = ReportHelper.getOrgAddressWithPhoneEmail(qc_data.shipment.ConignerAdrs_CompanyName, qc_data.shipment.ConignerAdrs_Address1, qc_data.shipment.ConignerAdrs_Address2, ReportHelper.getCityCountry(qc_data.shipment.ConignerAdrs_City, qc_data.shipment.ConignerAdrs_Country), qc_data.shipment.ConignerAdrs_Email, qc_data.shipment.ConignerAdrs_ContactPhone);
//        var consignee = ReportHelper.getOrgAddressWithPhoneEmail(qc_data.shipment.ConsigneeAdrs_CompanyName, qc_data.shipment.ConsigneeAdrs_Address1, qc_data.shipment.ConsigneeAdrs_Address2, ReportHelper.getCityCountry(qc_data.shipment.ConsigneeAdrs_City, qc_data.shipment.ConsigneeAdrs_Country), qc_data.shipment.ConsigneeAdrs_Email, qc_data.shipment.ConsigneeAdrs_ContactPhone);
//        var notify = ReportHelper.getOrgAddressWithPhoneEmail(qc_data.shipment.NotifyPartyAdrs_CompanyName, qc_data.shipment.NotifyPartyAdrs_Address1, qc_data.shipment.NotifyPartyAdrs_Address2, ReportHelper.getCityCountry(qc_data.shipment.NotifyPartyAdrs_City, qc_data.shipment.NotifyPartyAdrs_Country), qc_data.shipment.NotifyPartyAdrs_Email, qc_data.shipment.NotifyPartyAdrs_ContactPhone);
//
//        dictionary["ClientName"] = qc_data.shipment.ClientAdrs_CompanyName;
//        dictionary["Consigner"] = qc_data.shipment.ConsignerFullName;
//        dictionary["Consignee"] = qc_data.shipment.ConsigneeFullName;
//        dictionary["ConsignerAir"] = getCompleteNameAndAddress(qc_data.shipment.ConsignerFullName, consigner);
//        dictionary["ConsigneeAir"] = getCompleteNameAndAddress(qc_data.shipment.ConsigneeFullName, consignee);
//        dictionary["NotifyPartyAir"] = getCompleteNameAndAddress(qc_data.shipment.NotifyPartyFullName, notify);
//        dictionary["NotifyParty"] = notify;
//        dictionary["ConsignerAddress"] = ReportHelper.getAddressList(qc_data.shipment.ConignerAdrs_Address1);
//        dictionary["ConsigneeAddress"] = ReportHelper.getAddressList(qc_data.shipment.ConsigneeAddress1);
//        dictionary["NotifyPartyAddress"] = ReportHelper.getAddressList(qc_data.shipment.NotifyPartyAddress1);
//        dictionary["Tenant"] = ReportHelper.getListOfStrings(qc_data?.tenant?.TenantName, qc_data?.tenant?.Address1, qc_data?.tenant?.Address2, qc_data?.tenant?.City, qc_data?.tenant?.State, qc_data?.tenant?.ZipPostCode, qc_data?.tenant?.Country, qc_data?.tenant?.Email, qc_data?.tenant?.WebsiteUrl, qc_data?.tenant?.Phone);
//        dictionary["noofpackages"] = qc_data.shipment.Packs;
//        dictionary["noofpackages_word"] = qc_data.noofpackages_word;
//        dictionary["UserDisplayName"] = qc_data.userdisplayname;
//        dictionary["CurrentDate"] = Utils.ConvertToDPWDateFormat(System.DateTime.Now);
//        dictionary["DeliveryAgent"] = null;
//        if (qc_data.shipment.IsNotifyPartyFreeTextAddress.Value)
//        {
//            dictionary["NotifyPartyAddressFreeText"] = ReportHelper.getAddressList(qc_data.shipment.NotifyPartyFreeTextAddress);
//        }
//        else
//        {
//            dictionary["NotifyPartyAddressFreeText"] = notify;
//        }
//
//        if (qc_data.shipment.IsConsigneeFreeTextAddress.Value)
//        {
//            dictionary["ConsigneeAddressFreeText"] = ReportHelper.getAddressList(qc_data.shipment.ConsigneeFreeTextAddress);
//        }
//        else
//        {
//            dictionary["ConsigneeAddressFreeText"] = consignee;
//        }
//
//        if (qc_data.shipment.IsConsignerFreeTextAddress.Value)
//        {
//            dictionary["ConsignerAddressFreeText"] = ReportHelper.getAddressList(qc_data.shipment.ConsignerFreeTextAddress);
//        }
//        else
//        {
//            dictionary["ConsignerAddressFreeText"] = consigner;
//        }
//
//        if (deliveryAgentAddress != null && dictionary["DeliveryAgent"] == null)
//        {
//            var deliveryAgent = ReportHelper.getOrgAddressWithPhoneEmail(deliveryAgentAddress.CompanyName, deliveryAgentAddress.Address1, deliveryAgentAddress.Address2, getCityCountry(deliveryAgentAddress.City, deliveryAgentAddress.Country), deliveryAgentAddress.Email, deliveryAgentAddress.ContactPhone);
//            if(deliveryAgentAddress.OrgFullName != null) {
//                deliveryAgent.Insert(0, deliveryAgentAddress.OrgFullName);
//            }
//            dictionary["DeliveryAgent"] = deliveryAgent;
//        }
//
//        if(qc_data.shipment.Etd != null)
//            dictionary["Etd"] = Utils.ConvertToDPWDateFormat(qc_data.shipment.Etd);
//
//        if(qc_data.shipment.Eta != null)
//            dictionary["Eta"] = Utils.ConvertToDPWDateFormat(qc_data.shipment.Eta);
//
//        dictionary["ShipmentContainers"] = qc_data.shipment.ShipmentContainers;
//        dictionary[ReportConstants.CONTAINER_COUNT_BY_CODE] = ReportHelper.getCountByContainerTypeCode(allContainersList);
//
//        if(originDetail!=null && originDetail.IATACode!=null)
//            dictionary["Origin"] = originDetail.IATACode;
//        else
//            dictionary["Origin"] = null;
//        if(destinationDetail!=null && destinationDetail.IATACode!=null)
//            dictionary["Destination"] = destinationDetail.IATACode;
//        else
//            dictionary["Destination"] = null;
//        dictionary["JobNo"] = qc_data.shipment.ShipmentId;
//        dictionary["Airline"] = qc_data.shipment.Carrier;
//        dictionary["FlightNo"] = qc_data.shipment.FlightNumber;
//        dictionary["FlightName"] = qc_data.shipment.Carrier;
//        dictionary["MarksNo"] = qc_data.shipment.MarksnNums;
//        dictionary["Remarks"] = qc_data.shipment.Remarks;
//        if(qc_data.shipment.VolumeWeight != null)
//            dictionary["VWeightAndUnit"] = string.Format("{0} {1}", ReportHelper.twoDecimalPlacesFormat(qc_data.shipment.VolumeWeight.ToString()), qc_data.shipment.WeightUnit);
//        if(qc_data.shipment.Weight != null)
//            dictionary["WeightAndUnit"] = string.Format("{0} {1}", ReportHelper.twoDecimalPlacesFormat(qc_data.shipment.Weight.ToString()), qc_data.shipment.WeightUnit);
//        if(qc_data.shipment.Volume != null)
//            dictionary["VolumeAndUnit"] = string.Format("{0} {1}", ReportHelper.twoDecimalPlacesFormat(qc_data.shipment.Volume.ToString()), qc_data.shipment.VolumeUnit);
//        if(qc_data.shipment.Volume != null)
//            dictionary["TotalVolume"] = string.Format("{0} {1}", ReportHelper.twoDecimalPlacesFormat(qc_data.shipment.Volume.ToString()), qc_data.shipment.VolumeUnit);
//        if(qc_data.shipment.Weight != null)
//            dictionary["TotalWeight"] = string.Format("{0} {1}", ReportHelper.twoDecimalPlacesFormat(qc_data.shipment.Weight.ToString()), qc_data.shipment.WeightUnit);
//        dictionary["ToatalPcs"] = qc_data.noofpackages_word;
//        if(qc_data.shipment.POLPortName!=null)
//            dictionary["PortofDeparture"] = qc_data.shipment.POLPortName;
//        else
//            dictionary["PortofDeparture"] = null;
//        if(qc_data.shipment.POLCountry!=null)
//            dictionary["PortofDepartureCountry"] = qc_data.shipment.POLCountry;
//        else
//            dictionary["PortofDepartureCountry"] = null;
//        if(qc_data.shipment.PODPortName!=null)
//            dictionary["PortofArrival"] = qc_data.shipment.PODPortName;
//        else
//            dictionary["PortofArrival"] = null;
//        if(qc_data.shipment.PODCountry!=null)
//            dictionary["PortofArrivalCountry"] = qc_data.shipment.PODCountry;
//        else
//            dictionary["PortofArrivalCountry"] = null;
//        dictionary["PlaceofReceipt"] = qc_data.shipment.OriginName;
//        dictionary["PlaceofDelivery"] = qc_data.shipment.DestinationName;
//        if(consolRow!=null && consolRow.Payment!=null)
//            dictionary["PPCC"] = consolRow.Payment;
//        else
//            dictionary["PPCC"] = null;
//
//        if(packsDetails!=null && packsDetails.Count() > 0)
//        {
//            string json = JsonConvert.SerializeObject(packsDetails);
//            var values = JsonConvert.DeserializeObject<List<Dictionary<string, object>>>(json);
//            using(var connection = SqlConnections.NewByKey("Default"))
//            {
//                values.ForEach(v =>
//                        {
//                                PrintUtil.JsonDateFormat(ref v);
//                if(v.ContainsKey("CommodityName") && v["CommodityName"] != null)
//                {
//                    v["CommodityDesc"] = v["CommodityName"].ToString();
//                }
//                    });
//            }
//            dictionary["PacksDetails"] = values;
//        }
//        //dictionary["ContainerCount"] = GetNextNumberHelper.NumberToWords(qc_data.shipment.ShipmentContainers.Count).ToUpper();
//
//        //qc_data.IsMultipleCopy = true;
//        return dictionary;    }
//}
