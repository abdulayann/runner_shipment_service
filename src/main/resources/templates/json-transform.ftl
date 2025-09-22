{
    "bookingState": <#if data.status??>"${data.status}"<#else>null</#if>,
    <#if data.serviceType?? && (data.serviceType == "P2P" || data.serviceType == "F2F" || data.serviceType == "F2P" || data.serviceType == "P2F")>
    "moveType": <#if data.serviceType == "P2P">"PortToPort"<#elseif data.serviceType == "F2F">"DoorToDoor"<#elseif data.serviceType == "F2P">"DoorToPort"<#elseif data.serviceType == "P2F">"PortToDoor"</#if>,
    <#else>
    "moveType": null,
    </#if>
    "carrierReferenceNumber": <#if data.carrierBookingNo??>"${data.carrierBookingNo}"<#else>null</#if>,
    "parties": [
        {
            "partyRole": "Booker",
            "partyINTTRACompanyId": <#if data.requester?? && data.requester.orgData?? && data.requester.orgData.remoteIdType?? && data.requester.orgData.remoteIdType == "INTRA_COMPANY_ID">"${data.requester.orgData.remoteId!""}"<#else>null</#if>,
            "contacts": [
                {
                    "name": <#if data.requester?? && data.requester.orgData?? && data.requester.orgData.fullName??>"${data.requester.orgData.fullName}"<#else>null</#if>,
                    "contactType": "InformationContact",
                    <#if data.requester?? && data.requester.addressData?? && data.requester.addressData.phone??>"phones": ["${data.requester.addressData.phone}"],<#else>"phones": [],</#if>
                    <#if data.requester?? && data.requester.addressData?? && data.requester.addressData.email??>"emails": ["${data.requester.addressData.email}"]<#else>"emails": []</#if>
                },
                {
                    "name": "pushnotification",
                    "contactType": "NotificationContact",
                    "emails": [
                        <#-- Collect all unique emails -->
                        <#assign emailList = []>

                        <#-- Split externalEmails -->
                        <#if data.externalEmails??>
                            <#list data.externalEmails?split(",") as extEmail>
                                <#assign cleanEmail = extEmail?trim>
                                <#if cleanEmail?has_content && !emailList?seq_contains(cleanEmail)>
                                    <#assign emailList = emailList + [cleanEmail]>
                                </#if>
                            </#list>
                        </#if>

                        <#-- Split internalEmails -->
                        <#if data.internalEmails??>
                            <#list data.internalEmails?split(",") as intEmail>
                                <#assign cleanEmail = intEmail?trim>
                                <#if cleanEmail?has_content && !emailList?seq_contains(cleanEmail)>
                                    <#assign emailList = emailList + [cleanEmail]>
                                </#if>
                            </#list>
                        </#if>

                        <#-- Add createByUserEmail -->
                        <#if data.createByUserEmail??>
                            <#assign createByEmail = data.createByUserEmail?trim>
                            <#if createByEmail?has_content && !emailList?seq_contains(createByEmail)>
                                <#assign emailList = emailList + [createByEmail]>
                            </#if>
                        </#if>

                        <#-- Add submitByUserEmail -->
                        <#if data.submitByUserEmail??>
                            <#assign submitByEmail = data.submitByUserEmail?trim>
                            <#if submitByEmail?has_content && !emailList?seq_contains(submitByEmail)>
                                <#assign emailList = emailList + [submitByEmail]>
                            </#if>
                        </#if>

                        <#-- Output unique emails -->
                        <#list emailList as email>
                            "${email}"<#if email_has_next>,</#if>
                        </#list>
                    ]
                }
            ]
        },
        {
            "partyRole": "Carrier",
            <#if data.sailingInformation?? && data.sailingInformation.scacCode??>"partyAlias": "${data.sailingInformation.scacCode}",</#if>
            "partyINTTRACompanyId": null,
            "partyName1": <#if data.sailingInformation?? && data.sailingInformation.carrier??>"${data.sailingInformation.carrier}"<#else>""</#if>
        },
        {
            <#-- Shipper party with address -->
            "address": {
                <#if data.shipper?? && data.shipper.addressData??>
                <#-- Address processing macro -->
                <#assign address1 = (data.shipper.addressData.address1!)>
                <#assign address2 = (data.shipper.addressData.address2!)>
                <#assign fullAddress = (address1 + " " + address2)?trim>
                <#assign remainingAddress = fullAddress>
                <#assign addressLines = []>

                <#-- Split address into 35-character chunks -->
                <#list 1..4 as i>
                    <#if remainingAddress?length gt 0>
                        <#if remainingAddress?length gt 35>
                            <#assign currentLine = remainingAddress?substring(0, 35)>
                            <#assign remainingAddress = remainingAddress?substring(35)>
                            <#assign addressLines = addressLines + [currentLine]>
                        <#else>
                            <#assign addressLines = addressLines + [remainingAddress]>
                            <#assign remainingAddress = "">
                            <#break>
                        </#if>
                    </#if>
                </#list>

                <#-- Output address lines -->
                <#if addressLines?size gt 0>"unstructuredAddress01": "${addressLines[0]}",<#else>"unstructuredAddress01": null,</#if>
                <#if addressLines?size gt 1>"unstructuredAddress02": "${addressLines[1]}",</#if>
                <#if addressLines?size gt 2>"unstructuredAddress03": "${addressLines[2]}",</#if>
                <#if addressLines?size gt 3>"unstructuredAddress04": "${addressLines[3]}",</#if>

                <#-- If there's still remaining address, put it in street01 -->
                <#if remainingAddress?length gt 0>"street01": "${remainingAddress}",</#if>

                "country": {
                    "countryName": <#if data.shipper.addressData.country??>"${data.shipper.addressData.country}"<#else>null</#if>
                },
                "postalCode": <#if data.shipper.addressData.zipPostCode??>"${data.shipper.addressData.zipPostCode}"<#else>null</#if>,
                "city": <#if data.shipper.addressData.city??>"${data.shipper.addressData.city}"<#else>null</#if>
                <#else>
                "unstructuredAddress01": null,
                "country": {
                    "countryName": null
                },
                "postalCode": null,
                "city": null
                </#if>
            },
            "contacts": [
                {
                    "name": <#if data.shipper?? && data.shipper.orgData?? && data.shipper.orgData.fullName??>"${data.shipper.orgData.fullName}"<#else>null</#if>,
                    "contactType": "InformationContact",
                    <#if data.shipper?? && data.shipper.addressData?? && data.shipper.addressData.phone??>"phones": ["${data.shipper.addressData.phone}"],<#else>"phones": [],</#if>
                    <#if data.shipper?? && data.shipper.addressData?? && data.shipper.addressData.email??>"emails": ["${data.shipper.addressData.email}"]<#else>"emails": []</#if>
                }
            ],
            "partyName1": <#if data.shipper?? && data.shipper.orgData?? && data.shipper.orgData.fullName??>"${data.shipper.orgData.fullName}"<#else>""</#if>,
            "partyRole": "Shipper",
            "partyINTTRACompanyId": <#if data.shipper?? && data.shipper.orgData?? && data.shipper.orgData.remoteIdType?? && data.shipper.orgData.remoteIdType == "INTRA_COMPANY_ID">"${data.shipper.orgData.remoteId!""}"<#else>null</#if>
        },
        {
            <#-- Forwarder party with address -->
            "address": {
                <#if data.forwardingAgent?? && data.forwardingAgent.addressData??>
                <#-- Same address processing for forwarder -->
                <#assign address1 = (data.forwardingAgent.addressData.address1!)>
                <#assign address2 = (data.forwardingAgent.addressData.address2!)>
                <#assign fullAddress = (address1 + " " + address2)?trim>
                <#assign remainingAddress = fullAddress>
                <#assign addressLines = []>

                <#list 1..4 as i>
                    <#if remainingAddress?length gt 0>
                        <#if remainingAddress?length gt 35>
                            <#assign currentLine = remainingAddress?substring(0, 35)>
                            <#assign remainingAddress = remainingAddress?substring(35)>
                            <#assign addressLines = addressLines + [currentLine]>
                        <#else>
                            <#assign addressLines = addressLines + [remainingAddress]>
                            <#assign remainingAddress = "">
                            <#break>
                        </#if>
                    </#if>
                </#list>

                <#if addressLines?size gt 0>"unstructuredAddress01": "${addressLines[0]}",<#else>"unstructuredAddress01": null,</#if>
                <#if addressLines?size gt 1>"unstructuredAddress02": "${addressLines[1]}",</#if>
                <#if addressLines?size gt 2>"unstructuredAddress03": "${addressLines[2]}",</#if>
                <#if addressLines?size gt 3>"unstructuredAddress04": "${addressLines[3]}",</#if>

                <#if remainingAddress?length gt 0>"street01": "${remainingAddress}",</#if>

                "country": {
                    "countryName": <#if data.forwardingAgent.addressData.country??>"${data.forwardingAgent.addressData.country}"<#else>null</#if>
                },
                "postalCode": <#if data.forwardingAgent.addressData.zipPostCode??>"${data.forwardingAgent.addressData.zipPostCode}"<#else>null</#if>,
                "city": <#if data.forwardingAgent.addressData.city??>"${data.forwardingAgent.addressData.city}"<#else>null</#if>
                <#else>
                "unstructuredAddress01": null,
                "country": {
                    "countryName": null
                },
                "postalCode": null,
                "city": null
                </#if>
            },
            "contacts": [
                {
                    "name": <#if data.forwardingAgent?? && data.forwardingAgent.orgData?? && data.forwardingAgent.orgData.fullName??>"${data.forwardingAgent.orgData.fullName}"<#else>null</#if>,
                    "contactType": "InformationContact",
                    <#if data.forwardingAgent?? && data.forwardingAgent.addressData?? && data.forwardingAgent.addressData.phone??>"phones": ["${data.forwardingAgent.addressData.phone}"],<#else>"phones": [],</#if>
                    <#if data.forwardingAgent?? && data.forwardingAgent.addressData?? && data.forwardingAgent.addressData.email??>"emails": ["${data.forwardingAgent.addressData.email}"]<#else>"emails": []</#if>
                }
            ],
            "partyName1": <#if data.forwardingAgent?? && data.forwardingAgent.orgData?? && data.forwardingAgent.orgData.fullName??>"${data.forwardingAgent.orgData.fullName}"<#else>""</#if>,
            "partyRole": "Forwarder",
            "partyINTTRACompanyId": <#if data.forwardingAgent?? && data.forwardingAgent.orgData?? && data.forwardingAgent.orgData.remoteIdType?? && data.forwardingAgent.orgData.remoteIdType == "INTRA_COMPANY_ID">"${data.forwardingAgent.orgData.remoteId!""}"<#else>null</#if>
        },
        {
            <#-- Consignee party with address -->
            "address": {
                <#if data.consignee?? && data.consignee.addressData??>
                <#assign address1 = (data.consignee.addressData.address1!)>
                <#assign address2 = (data.consignee.addressData.address2!)>
                <#assign fullAddress = (address1 + " " + address2)?trim>
                <#assign remainingAddress = fullAddress>
                <#assign addressLines = []>

                <#list 1..4 as i>
                    <#if remainingAddress?length gt 0>
                        <#if remainingAddress?length gt 35>
                            <#assign currentLine = remainingAddress?substring(0, 35)>
                            <#assign remainingAddress = remainingAddress?substring(35)>
                            <#assign addressLines = addressLines + [currentLine]>
                        <#else>
                            <#assign addressLines = addressLines + [remainingAddress]>
                            <#assign remainingAddress = "">
                            <#break>
                        </#if>
                    </#if>
                </#list>

                <#if addressLines?size gt 0>"unstructuredAddress01": "${addressLines[0]}",<#else>"unstructuredAddress01": null,</#if>
                <#if addressLines?size gt 1>"unstructuredAddress02": "${addressLines[1]}",</#if>
                <#if addressLines?size gt 2>"unstructuredAddress03": "${addressLines[2]}",</#if>
                <#if addressLines?size gt 3>"unstructuredAddress04": "${addressLines[3]}",</#if>

                <#if remainingAddress?length gt 0>"street01": "${remainingAddress}",</#if>

                "country": {
                    "countryName": <#if data.consignee.addressData.country??>"${data.consignee.addressData.country}"<#else>null</#if>
                },
                "postalCode": <#if data.consignee.addressData.zipPostCode??>"${data.consignee.addressData.zipPostCode}"<#else>null</#if>,
                "city": <#if data.consignee.addressData.city??>"${data.consignee.addressData.city}"<#else>null</#if>
                <#else>
                "unstructuredAddress01": null,
                "country": {
                    "countryName": null
                },
                "postalCode": null,
                "city": null
                </#if>
            },
            "contacts": [
                {
                    "name": <#if data.consignee?? && data.consignee.orgData?? && data.consignee.orgData.fullName??>"${data.consignee.orgData.fullName}"<#else>null</#if>,
                    "contactType": "InformationContact",
                    <#if data.consignee?? && data.consignee.addressData?? && data.consignee.addressData.phone??>"phones": ["${data.consignee.addressData.phone}"],<#else>"phones": [],</#if>
                    <#if data.consignee?? && data.consignee.addressData?? && data.consignee.addressData.email??>"emails": ["${data.consignee.addressData.email}"]<#else>"emails": []</#if>
                }
            ],
            "partyName1": <#if data.consignee?? && data.consignee.orgData?? && data.consignee.orgData.fullName??>"${data.consignee.orgData.fullName}"<#else>""</#if>,
            "partyRole": "Consignee",
            "partyINTTRACompanyId": <#if data.consignee?? && data.consignee.orgData?? && data.consignee.orgData.remoteIdType?? && data.consignee.orgData.remoteIdType == "INTRA_COMPANY_ID">"${data.consignee.orgData.remoteId!""}"<#else>null</#if>
        },
        {
            <#-- ContractParty with address -->
             "address": {
                <#if data.contract?? && data.contract.addressData??>
                <#assign address1 = (data.contract.addressData.address1!)>
                <#assign address2 = (data.contract.addressData.address2!)>
                <#assign fullAddress = (address1 + " " + address2)?trim>
                <#assign remainingAddress = fullAddress>
                <#assign addressLines = []>

                <#list 1..4 as i>
                    <#if remainingAddress?length gt 0>
                        <#if remainingAddress?length gt 35>
                            <#assign currentLine = remainingAddress?substring(0, 35)>
                            <#assign remainingAddress = remainingAddress?substring(35)>
                            <#assign addressLines = addressLines + [currentLine]>
                        <#else>
                            <#assign addressLines = addressLines + [remainingAddress]>
                            <#assign remainingAddress = "">
                            <#break>
                        </#if>
                    </#if>
                </#list>

                <#if addressLines?size gt 0>"unstructuredAddress01": "${addressLines[0]}",<#else>"unstructuredAddress01": null,</#if>
                <#if addressLines?size gt 1>"unstructuredAddress02": "${addressLines[1]}",</#if>
                <#if addressLines?size gt 2>"unstructuredAddress03": "${addressLines[2]}",</#if>
                <#if addressLines?size gt 3>"unstructuredAddress04": "${addressLines[3]}",</#if>

                <#if remainingAddress?length gt 0>"street01": "${remainingAddress}",</#if>

                "country": {
                    "countryName": <#if data.contract.addressData.country??>"${data.contract.addressData.country}"<#else>null</#if>
                },
                "postalCode": <#if data.contract.addressData.zipPostCode??>"${data.contract.addressData.zipPostCode}"<#else>null</#if>,
                "city": <#if data.contract.addressData.city??>"${data.contract.addressData.city}"<#else>null</#if>
                <#else>
                "unstructuredAddress01": null,
                "country": {
                    "countryName": null
                },
                "postalCode": null,
                "city": null
                </#if>
            },
            "contacts": [
                {
                    "name": <#if data.contract?? && data.contract.orgData?? && data.contract.orgData.fullName??>"${data.contract.orgData.fullName}"<#else>null</#if>,
                    "contactType": "InformationContact",
                    <#if data.contract?? && data.contract.addressData?? && data.contract.addressData.phone??>"phones": ["${data.contract.addressData.phone}"],<#else>"phones": [],</#if>
                    <#if data.contract?? && data.contract.addressData?? && data.contract.addressData.email??>"emails": ["${data.contract.addressData.email}"]<#else>"emails": []</#if>
                }
            ],
            "partyName1": <#if data.contract?? && data.contract.orgData?? && data.contract.orgData.fullName??>"${data.contract.orgData.fullName}"<#else>""</#if>,
            "partyRole": "ContractParty",
            "partyINTTRACompanyId": <#if data.contract?? && data.contract.orgData?? && data.contract.orgData.remoteIdType?? && data.contract.orgData.remoteIdType == "INTRA_COMPANY_ID">"${data.contract.orgData.remoteId!""}"<#else>null</#if>
        }
        <#-- Dynamic additional parties based on type -->
        <#if data.additionalParties??>
        <#assign hasMatchingParties = false>
        <#list data.additionalParties as party>
            <#if party.type?? && (party.type == "Notify Party" || party.type == "Notify Part 1" || party.type == "Notify Part 2" || party.type == "Customs Broker")>
                <#assign hasMatchingParties = true>
                <#break>
            </#if>
        </#list>
        <#if hasMatchingParties>
        <#list data.additionalParties as party>
        <#if party.type?? && (party.type == "Notify Party" || party.type == "Notify Part 1" || party.type == "Notify Part 2" || party.type == "Customs Broker")>
        ,{
            <#-- Address processing for additional parties -->
            <#if party.addressData??>
            "address": {
                <#assign partyAddress1 = (party.addressData.address1!)>
                <#assign partyAddress2 = (party.addressData.address2!)>
                <#assign partyFullAddress = (partyAddress1 + " " + partyAddress2)?trim>
                <#assign partyRemainingAddress = partyFullAddress>
                <#assign partyAddressLines = []>

                <#list 1..4 as i>
                    <#if partyRemainingAddress?length gt 0>
                        <#if partyRemainingAddress?length gt 35>
                            <#assign currentLine = partyRemainingAddress?substring(0, 35)>
                            <#assign partyRemainingAddress = partyRemainingAddress?substring(35)>
                            <#assign partyAddressLines = partyAddressLines + [currentLine]>
                        <#else>
                            <#assign partyAddressLines = partyAddressLines + [partyRemainingAddress]>
                            <#assign partyRemainingAddress = "">
                            <#break>
                        </#if>
                    </#if>
                </#list>

                <#if partyAddressLines?size gt 0>"unstructuredAddress01": "${partyAddressLines[0]}",<#else>"unstructuredAddress01": null,</#if>
                <#if partyAddressLines?size gt 1>"unstructuredAddress02": "${partyAddressLines[1]}",</#if>
                <#if partyAddressLines?size gt 2>"unstructuredAddress03": "${partyAddressLines[2]}",</#if>
                <#if partyAddressLines?size gt 3>"unstructuredAddress04": "${partyAddressLines[3]}",</#if>

                <#if partyRemainingAddress?length gt 0>"street01": "${partyRemainingAddress}",</#if>

                "country": {
                    "countryName": <#if party.addressData.country??>"${party.addressData.country}"<#else>null</#if>
                },
               "postalCode": <#if party.addressData.zipPostCode??>"${party.addressData.zipPostCode}"<#else>null</#if>,
               "city": <#if party.addressData.city??>"${party.addressData.city}"<#else>null</#if>
            },
            </#if>
            "contacts": [
                {
                    "name": <#if party.orgData?? && party.orgData.fullName??>"${party.orgData.fullName}"<#else>null</#if>,
                    "contactType": "InformationContact",
                    <#if party.addressData?? && party.addressData.phone??>"phones": ["${party.addressData.phone}"],<#else>"phones": [],</#if>
                    <#if party.addressData?? && party.addressData.email??>"emails": ["${party.addressData.email}"]<#else>"emails": []</#if>
                }
            ],
            "partyName1": <#if party.orgData?? && party.orgData.fullName??>"${party.orgData.fullName}"<#else>""</#if>,
            <#-- Map party types to partyRole -->
            "partyRole": <#if party.type == "Notify Party">"MainNotifyParty"<#elseif party.type == "Notify Part 1">"FirstAdditionalNotifyParty"<#elseif party.type == "Notify Part 2">"SecondAdditionalNotifyParty"<#elseif party.type == "Customs Broker">"CustomsBroker"<#else>null</#if>,
            "partyINTTRACompanyId": <#if party.orgData?? && party.orgData.remoteIdType?? && party.orgData.remoteIdType == "INTRA_COMPANY_ID">"${party.orgData.remoteId!""}"<#else>null</#if>
        }
        </#if>
        </#list>
        </#if>
        </#if>
    ],
    "messageDate": {
        "dateFormat": "CCYYMMDDHHMM",
        "dateValue": "${.now?string('yyyyMMddHHmm')}"
    },
    "creationDate": {
        "dateFormat": "CCYYMMDDHHMM",
        "dateValue": "${.now?string('yyyyMMddHHmm')}"
    },
    "transactionContact": {
        "name": <#if data.createdBy??>"${data.createdBy}"<#else>""</#if>,
        "contactType": "InformationContact",
        <#if data.createByUserEmail??>"emails": ["${data.createByUserEmail}"],<#else>"emails": [],</#if>
        "phones": []
    },
    "cargoHazardousIndicator": false,
    "cargoEnvironmentalPollutantIndicator": false,
    "cargoOutofGaugeIndicator": false,
    <#if data.bookingComment??>"generalComments": ["${data.bookingComment}"],<#else>"generalComments": [],</#if>
    "notifyMe": true,
    "transactionLocations": [
        {
            "locationType": "PlaceOfReceipt",
            "identifierType": "UNLOC",
            "identifierValue": <#if data.sailingInformation?? && data.sailingInformation.carrierReceiptPlace??>"${data.sailingInformation.carrierReceiptPlace}"<#else>null</#if>,
            "locationDates": [
                {
                    "type": "EarliestDepartureDate",
                    "dateFormat": "CCYYMMDD",
                    "dateValue": <#if data.sailingInformation?? && data.sailingInformation.earliestDepartureDate??>"${data.sailingInformation.earliestDepartureDate?datetime.iso?string('yyyyMMdd')}"
                    <#else>
                      null
                    </#if>
                }
            ]
        },
        {
            "locationType": "PlaceOfDelivery",
            "identifierType": "UNLOC",
            "identifierValue": <#if data.sailingInformation?? && data.sailingInformation.carrierDeliveryPlace??>"${data.sailingInformation.carrierDeliveryPlace}"<#else>null</#if>,
            "locationDates": [
                {
                    "type": "LatestDeliveryDate",
                    "dateFormat": "CCYYMMDD",
                    "dateValue": <#if data.sailingInformation?? && data.sailingInformation.latestDeliveryDate??>"${data.sailingInformation.latestDeliveryDate?datetime.iso?string('yyyyMMdd')}"
                    <#else>
                      null
                    </#if>
                }
            ]
        },
        {
            "locationType": "BookingOffice",
            "identifierType": "UNLOC",
            "identifierValue": <#if data.bookingOffice??>"${data.bookingOffice}"<#else>null</#if>
        }
    ],
    "references": [
        {
            "referenceType": "FreightForwarderRefNumber",
            "referenceValue": <#if data.entityId??>"${data.entityId}"<#else>null</#if>
        }
        <#if data.referenceNumbersList?? && data.referenceNumbersList?has_content>
        <#list data.referenceNumbersList as ref>
        <#if ref.type == "CON" || ref.type == "SRN" || ref.type == "PON" || ref.type == "CPR" || ref.type == "CRN">
        ,{
            <#if ref.type == "CON">"referenceType": "ContractNumber"<#elseif ref.type == "SRN">"referenceType": "ShipperReferenceNumber"<#elseif ref.type == "PON">"referenceType": "PurchaseOrderNumber"<#elseif ref.type == "CPR">"referenceType": "ContractPartyReferenceNumber"<#elseif ref.type == "CRN">"referenceType": "ConsigneeReferenceNumber"</#if>,
            "referenceValue": <#if ref.referenceNumber??>"${ref.referenceNumber}"<#else>null</#if>
        }
        </#if>
        </#list>
        </#if>
    ],
    <#if data.sailingInformation??>
    "transportLegs": [
        {
            "stage": "MainCarriage",
            "vesselName": <#if data.sailingInformation.vesselName??>"${data.sailingInformation.vesselName}"<#else>null</#if>,
            "conveyanceNumber": <#if data.sailingInformation.voyageNo??>"${data.sailingInformation.voyageNo}"<#else>null</#if>,
            "startLocation": {
                "locationType": "PlaceOfLoad",
                "identifierType": "UNLOC",
                "identifierValue": <#if data.sailingInformation.pol??>"${data.sailingInformation.pol}"<#else>null</#if>,
                "locationDates": [
                    {
                        "dateValue": <#if data.sailingInformation?? && data.sailingInformation.etd??>"${data.sailingInformation.etd?datetime.iso?string('yyyyMMdd')}"<#else>null</#if>,
                        "dateFormat": "CCYYMMDD",
                        "type": "EstimatedDepartureDate"
                    }
                ]
            },
            "endLocation": {
                "locationType": "PlaceOfDischarge",
                "identifierType": "UNLOC",
                "identifierValue": <#if data.sailingInformation.pod??>"${data.sailingInformation.pod}"<#else>null</#if>,
                "locationDates": [
                    {
                        "dateValue": <#if data.sailingInformation?? && data.sailingInformation.eta??>"${data.sailingInformation.eta?datetime.iso?string('yyyyMMdd')}"<#else>null</#if>,
                        "dateFormat": "CCYYMMDD",
                        "type": "EstimatedArrivalDate"
                    }
                ]
            },
            "mode": "MaritimeTransport"
        }
    ],
    <#else>
    "transportLegs": [],
    </#if>
    <#if data.containersList??>
    "packageDetails": [
        <#list data.containersList as container>
        {
            "lineNumber": ${container_index + 1},
            "packageType": "OUTER",
            "goodsDescription": <#if container.goodsDescription??>"${container.goodsDescription}"<#else>null</#if>,
            "goodsGrossVolume": {
                "volumeType": "MTQ",
                "volumeValue": <#if container.volume??>"${container.volume?string('0.000')}"<#else>null</#if>
            },
            "goodsGrossWeight": {
                "weightType": "KGM",
                "weightValue": <#if container.grossWeight??>"${container.grossWeight?string('0.000')}"<#else>null</#if>
            },
            "goodsNetWeight": {
                "weightType" : <#if  container.netWeightUnit??>"${container.netWeightUnit}"<#else>null</#if>,
                "weightValue": <#if container.netWeight??>"${container.netWeight?string('0.000')}"<#else>null</#if>
            },
            "goodsClassificationType": "WCO",
            "goodsClassificationValue": <#if container.hsCode??>"${container.hsCode}"<#else>null</#if>
        }<#if container_has_next>,</#if>
        </#list>
    ],
    <#else>
    "packageDetails": [],
    </#if>
    <#if data.containersList??>
    "equipments": [
        <#list data.containersList as container>
        {
            "comments": [],
            "count": 1,
            "serviceType": "FCLFCL",
            "equipmentSizeCode": {
                "sizeCodeType": "ISO",
                "sizeCodeValue": <#if container.containerCode??>"${container.containerCode}"<#else>null</#if>,
                "sizeCodeDescription": null
            },
            "haulage": {
                <#if data.serviceType??>
                    <#if data.serviceType == "F2F">"arrangement": "CarrierCarrier"<#elseif data.serviceType == "F2P">"arrangement": "CarrierMerchant"<#elseif data.serviceType == "P2F">"arrangement": "MerchantCarrier"<#elseif data.serviceType == "P2P">"arrangement": "MerchantMerchant"<#else>"arrangement": "MerchantMerchant"</#if>,
                <#else>"arrangement": "MerchantMerchant",</#if>
                "points": [
                    <#if data.serviceType?? && (data.serviceType == "F2F" || data.serviceType == "P2F")>
                    {
                        "haulageParty": {
                            "address": {
                                <#if data.deliveryTo?? && data.deliveryTo.addressData??>
                                <#assign deliveryAddress1 = (data.deliveryTo.addressData.address1!)>
                                <#assign deliveryAddress2 = (data.deliveryTo.addressData.address2!)>
                                <#assign deliveryFullAddress = (deliveryAddress1 + " " + deliveryAddress2)?trim>
                                <#assign deliveryRemainingAddress = deliveryFullAddress>
                                <#assign deliveryAddressLines = []>

                                <#list 1..4 as i>
                                    <#if deliveryRemainingAddress?length gt 0>
                                        <#if deliveryRemainingAddress?length gt 35>
                                            <#assign currentLine = deliveryRemainingAddress?substring(0, 35)>
                                            <#assign deliveryRemainingAddress = deliveryRemainingAddress?substring(35)>
                                            <#assign deliveryAddressLines = deliveryAddressLines + [currentLine]>
                                        <#else>
                                            <#assign deliveryAddressLines = deliveryAddressLines + [deliveryRemainingAddress]>
                                            <#assign deliveryRemainingAddress = "">
                                            <#break>
                                        </#if>
                                    </#if>
                                </#list>

                                <#if deliveryAddressLines?size gt 0>"unstructuredAddress01": "${deliveryAddressLines[0]}",<#else>"unstructuredAddress01": null,</#if>
                                <#if deliveryAddressLines?size gt 1>"unstructuredAddress02": "${deliveryAddressLines[1]}",</#if>
                                <#if deliveryAddressLines?size gt 2>"unstructuredAddress03": "${deliveryAddressLines[2]}",</#if>
                                <#if deliveryAddressLines?size gt 3>"unstructuredAddress04": "${deliveryAddressLines[3]}",</#if>

                                <#if deliveryRemainingAddress?length gt 0>"street01": "${deliveryRemainingAddress}",</#if>

                                "country": {
                                    "countryName": <#if data.deliveryTo.addressData.country??>"${data.deliveryTo.addressData.country}"<#else>null</#if>
                                },
                                "postalCode": <#if data.deliveryTo.addressData.zipPostCode??>"${data.deliveryTo.addressData.zipPostCode}"<#else>null</#if>,
                                <#if data.deliveryTo.addressData.city??>"city": "${data.deliveryTo.addressData.city}"</#if>
                                <#else>
                                "unstructuredAddress01": null,
                                "country": {
                                     "countryName": null
                                },
                                "postalCode": null,
                                "city": null
                                </#if>
                            },
                            "contacts": [
                                {
                                    "name": <#if data.deliveryToContactName??>"${data.deliveryToContactName}"<#else>null</#if>,
                                    "contactType": "InformationContact",
                                    <#if data.deliveryToContactNo??>"phones": ["${data.deliveryToContactNo}"]<#else>"phones": []</#if>
                                }
                            ],
                            "partyName1": <#if data.deliveryTo?? && data.deliveryTo.orgData?? && data.deliveryTo.orgData.fullName??>"${data.deliveryTo.orgData.fullName}"<#else>""</#if>,
                            "partyRole": "ShipTo"
                        },
                        "dates": [
                            {
                                "dateValue": <#if data.deliveryToReqFullPickupDate??>"${data.deliveryToReqFullPickupDate?string('yyyyMMdd')}"<#else>null</#if>,
                                "haulageDateType": "RequestedDoorDeliveryDate",
                                "dateFormat": "CCYYMMDD"
                            }
                        ]
                    },
                    </#if>
                    <#if data.serviceType?? && (data.serviceType == "F2F" || data.serviceType == "F2P")>
                    <#if data.serviceType == "F2F">,</#if>
                    {
                        "haulageParty": {
                            <#if data.pickupFrom?? && data.pickupFrom.addressData??>
                            "address": {
                                <#assign pickupAddress1 = (data.pickupFrom.addressData.address1!)>
                                <#assign pickupAddress2 = (data.pickupFrom.addressData.address2!)>
                                <#assign pickupFullAddress = (pickupAddress1 + " " + pickupAddress2)?trim>
                                <#assign pickupRemainingAddress = pickupFullAddress>
                                <#assign pickupAddressLines = []>

                                <#list 1..4 as i>
                                    <#if pickupRemainingAddress?length gt 0>
                                        <#if pickupRemainingAddress?length gt 35>
                                            <#assign currentLine = pickupRemainingAddress?substring(0, 35)>
                                            <#assign pickupRemainingAddress = pickupRemainingAddress?substring(35)>
                                            <#assign pickupAddressLines = pickupAddressLines + [currentLine]>
                                        <#else>
                                            <#assign pickupAddressLines = pickupAddressLines + [pickupRemainingAddress]>
                                            <#assign pickupRemainingAddress = "">
                                            <#break>
                                        </#if>
                                    </#if>
                                </#list>

                                <#if pickupAddressLines?size gt 0>"unstructuredAddress01": "${pickupAddressLines[0]}",<#else>"unstructuredAddress01": null,</#if>
                                <#if pickupAddressLines?size gt 1>"unstructuredAddress02": "${pickupAddressLines[1]}",</#if>
                                <#if pickupAddressLines?size gt 2>"unstructuredAddress03": "${pickupAddressLines[2]}",</#if>
                                <#if pickupAddressLines?size gt 3>"unstructuredAddress04": "${pickupAddressLines[3]}",</#if>

                                <#if pickupRemainingAddress?length gt 0>"street01": "${pickupRemainingAddress}",</#if>

                                "country": {
                                    "countryName": <#if data.pickupFrom.addressData.country??>"${data.pickupFrom.addressData.country}"<#else>null</#if>
                                },
                                "postalCode": <#if data.pickupFrom.addressData.zipPostCode??>"${data.pickupFrom.addressData.zipPostCode}"<#else>null</#if>,
                                <#if data.pickupFrom.addressData.city??>"city": "${data.pickupFrom.addressData.city}"</#if>
                                <#else>
                                "unstructuredAddress01": null,
                                "country": {
                                     "countryName": null
                                },
                                "postalCode": null,
                                "city": null
                                </#if>
                            },
                            "contacts": [
                                {
                                    "name": <#if data.pickupFromContactName??>"${data.pickupFromContactName}"<#else>null</#if>,
                                    "contactType": "InformationContact",
                                    <#if data.pickupFromContactNo??>"phones": ["${data.pickupFromContactNo}"]<#else>"phones": []</#if>
                                }
                            ],
                            "partyName1": <#if data.pickupFrom?? && data.pickupFrom.orgData?? && data.pickupFrom.orgData.fullName??>"${data.pickupFrom.orgData.fullName}"<#else>""</#if>,
                            "partyRole": "ShipFrom"
                        },
                        "dates": [
                            {
                                "dateValue": <#if data.pickupFromReqEmptyPositioningDate??>"${data.pickupFromReqEmptyPositioningDate?string('yyyyMMdd')}"<#else>null</#if>,
                                "haulageDateType": "EmptyPositioningDate",
                                "dateFormat": "CCYYMMDD"
                            },
                            {
                                "dateValue": <#if data.pickupFromReqFullPickupDate??>"${data.pickupFromReqFullPickupDate?string('yyyyMMdd')}"<#else>null</#if>,
                                "haulageDateType": "FullPickUpDateTime",
                                "dateFormat": "CCYYMMDD"
                            }
                        ]
                    }
                    </#if>
                ]
            }
        }<#if container_has_next>,</#if>
        </#list>
    ]
    <#else>
    "equipments": []
    </#if>
}