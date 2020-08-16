#' Generate a 3D autostereogram of a dinosaur
#'
#' Pulls a random dinosaur (or other) image using rphylopic to generate a magic eye.
#'
#' @param colors Array of at least 2 color hex codes. Colors should be visually distinct and avoid using too many.
#' @param dino UID of an image from phylopic.org. See \code{rphylopic} package.
#' @return A raster matrix for \code{plot()}.
#' @export
#' @examples
#'
#'  dino_to_magiceye() %>% plot()
#'

dino_to_magiceye <- function(colors = c("#449999", "#FFFFFF", "#010101"),
                             dino = NULL){

  if(!requireNamespace("rphylopic", quietly = TRUE)){
    stop("Please install the rphylopic package to use this function.")
  }

  if(is.null(dino)){
    random_dino() %>%
      rphylopic::image_data(size = 512) -> phylo_dino
  } else {
    dino %>%
      rphylopic::image_data(size = 512) -> phylo_dino
  }


  phylo_dino[[1]] -> img

  #If image has fewer rows (y) than columns (x), transpose
  trans_image <- dim(img)[1] >= dim(img)[2]
  if(trans_image){
    img <- aperm(img, c(2, 1, 3))
  }

  #Pad image
  # First to make square, then to add some padding
  diff = dim(img)[2] - dim(img)[1]

  filler <- array(0, dim = c(round(diff/2), dim(img)[2], 4))
  img2 <- abind::abind(filler, img, filler, along = 1)

  #Second pad the sides so we can see full image
  filler2 <- array(0, dim = c(dim(img2)[1], round(dim(img2)[2]/5), 4))
  img3 <- abind::abind(filler2, img2, filler2, along = 2)

  if(trans_image){
    img3 <- aperm(img3, c(2, 1, 3))
  }

  img3 %>% image_to_magiceye(colors = colors, alpha_only = TRUE)

}


random_dino <- function(){
  dlist <- c("5f1e1adf-372c-4289-bfc5-147245c41170", "fd22540d-29a4-4b95-8a7e-ec9ab81f031d",
    "5b062105-b6a2-4405-bd75-3d0399102b9a", "fb405890-27fb-4dbc-b1e5-e7f38a185404",
    "173f44e3-2db7-4c98-b41f-6ab58093a145", "580db433-c610-40df-b7d8-a3434ad158ed",
    "1730665f-ffb8-4026-b9a6-e36ba92bbc24", "9161bbeb-fa38-48f6-bb20-c68c255dd9b6",
    "bf68e21e-fa8d-4f26-be72-03c52ce4d047", "6655838b-ba6a-4889-92f4-879444d1b931",
    "8c13fc28-3e54-4ea2-851c-1112e2bbf1fe", "f8df3f09-64ed-4982-8b96-ffc4996c22d1",
    "c8fa6626-4d1c-464e-9168-823bf6436358", "5b7e6fe2-3c73-4b1f-ac7b-e05655116b47",
    "3e436cff-6601-4629-a99e-ba0adeec3472", "b48a256b-cb24-4a48-8aa4-bd7f87ccf5db",
    "76be7ad0-6426-4ffa-92a3-07b1acbf4709", "4d160c01-0026-4176-9471-4e79771c8c91",
    "b7879e57-baca-4cd2-83c5-e85e8971a85f", "c5a448bf-0119-4735-8366-c237d855a1a0",
    "ddbd5048-3148-4088-9ab1-97becdcb7ae3", "540e9a55-77c9-4754-ad84-3ec003c6ace8",
    "efa0966c-65ca-433e-9e95-e4e18c15c790", "d7da9be9-fad6-4683-bf5e-bf7c951ee92c",
    "72d740f3-17f2-460f-b04b-fa9bb6b3e00f", "b5131da6-f158-46bd-812d-5cda4a0a2254",
    "7cbd4cf9-8869-46a7-93c7-71702b434309", "9f34bd6e-160b-4efc-91fd-d269441dd544",
    "f170274b-c887-4aea-a30c-73238ede3e4c", "fe24c8b2-a979-407e-a5fb-ba3a785e57f7",
    "ab7194d3-1d74-4029-a6b1-81663a25dc13", "e551c7e1-64e0-4ed0-ab36-d539ba988ac4",
    "6ecdac12-8832-401b-b6e2-8474dbc41b6a", "1677c9e0-e484-4db5-9693-dae9475bb270",
    "e1cf00c8-81b8-41c8-8fd1-c5f215c5bb53", "be5d1a4b-c6e8-4dbc-a8d6-0680abf886af",
    "b82711c1-4c4c-4fdd-b9ff-2bcd48aa4cd1", "85008e98-2194-4844-bff9-4c5513ddd117",
    "6105ff49-0bd0-477c-8bef-41b0c39fdca9", "9dc23409-1e36-402d-8be5-6b939f7fb664",
    "dc73bebc-7294-493d-b64a-98addd6ea837", "8c90fb70-ec89-4cc7-9675-8e6f034e8d02",
    "5f83d70e-a91f-437d-bee4-a4fc7bec995e", "06662a5b-a115-4231-897a-a74570b1320f",
    "f9e142f8-a58b-43d7-b241-26a6f755e5b6", "8c90fb70-ec89-4cc7-9675-8e6f034e8d02",
    "ea4d2a4d-3b4c-450a-bbdd-22520f7ef358", "4745210a-93f6-4bec-af63-e84820e9ea5b",
    "9e2c9596-43e9-4ea4-a642-93ee44501e73", "0f195f49-b914-4e91-a2c8-709de3a9b996",
    "e175b06d-60c1-4d0b-8596-fd2543738d19", "948449df-8b08-4ea4-9f1e-391cd410c78a",
    "d3ae8c0d-107d-45de-992e-177c551e079d", "58982e27-5545-4357-86d5-cb98b29ba853",
    "585a10c9-41ee-4c65-9c55-894c8fbe57f1", "14d3a680-60e4-4b9a-a6c4-4c85fb6527cc",
    "b1cb08ee-062d-4101-b945-014e654f4316", "7c6775fb-5df6-431c-9fcb-4e58a833bc29",
    "14184e4e-0777-4d0b-8cb4-00dedd216af4", "303ddff0-6a51-4f70-adf7-69bb22385d6d",
    "a46edfc9-7ee5-4316-9862-c27c435cf047", "83c83962-d551-42dd-a3b2-99346a89fc99",
    "4391e695-92b0-4e67-be00-4116f8af9fd0", "05d79645-d985-494d-a239-0e142c46d203",
    "c557874a-1438-4189-b364-1bb0db13effe", "79d397db-f503-48a9-853a-3357458b76df",
    "42bafc80-610d-4c5b-9553-771974ea6102", "baf9b47e-5ec9-45e9-828e-c6e206e8e95a",
    "f327468f-0fe9-4416-9ca8-fff9f0695444", "0ba39ffb-84b6-4516-97ac-8f8eb6fefc5a",
    "665dc6e9-3eee-4fd5-81fb-02d45271e9df", "7d36a10c-43ff-41c5-a939-f3fd203d419c",
    "23027ce7-713d-4c28-97e2-aec0219266df", "e401eed5-29f1-42c8-abbd-1f18134734f6",
    "812299bb-c476-4b9a-a918-6119f96ff1e3", "c59710f4-3ee4-4cfe-93bb-913b42cec4c8",
    "230b9d10-9efd-459f-bd2c-89e9cdef997b", "df3e8dab-47c2-4734-b050-dfee5685cbfd",
    "f91c54b6-ea6e-46d8-9035-e8da65c1db1f", "9a9cffa6-b151-4260-babd-a66b1237d506",
    "88179cbe-c1ab-4367-9d17-86047f547bc9", "4d0a3cfd-379a-443d-ae5e-07058e2d9055",
    "aa9f6082-6b87-4f13-b6f5-9c4756289637", "16326846-ecd5-49a9-b450-176d3f63703e",
    "f38583f2-b978-4bfe-afba-7bcd18efac60", "36c77c59-0069-4b2a-b2ae-a213cd94ab1d",
    "79f26be5-29d6-4a8e-a2bf-cd3a15fa8e6f", "f73a57a3-2489-44fe-9b26-d213d20ed8d0",
    "97ce8bbd-68a1-4185-a23e-59dcad7d7f15", "41039e8c-2baa-489e-9a0d-b6798f9b79e0",
    "ef275bb4-d7bc-4687-a52e-9dab12a1f582", "918ad1f7-fc6e-43f5-9708-b2da8a71a820",
    "40de8560-4b47-4707-823d-244e2713bbde", "1179b795-27b3-4c2a-a922-a3dbbe1e44df",
    "7a99b167-b719-4233-946c-addf3ef1c06c", "a5047982-35ad-4d7c-86c7-9f6232d4b2b2",
    "26245351-d851-4ee0-8a85-1740e798ef43", "62caaa0d-691d-4cff-8f2c-5cf8c230de95",
    "0e465876-924d-4da6-befe-3dddc729ae98", "6ef5f66c-5bd8-491c-8569-c33576467d70",
    "054a0b43-ac08-473a-841e-725bc757cdee", "e4129f4f-942e-4c5e-9b81-07542c7b3deb",
    "11491d2a-337d-41ae-a276-ebdc18e5055b", "0a4e1b13-d4f4-425c-8b80-8db24831280f",
    "5fb2a2f9-8712-486c-ac76-bcdc0e14c2bd", "e093cadb-cec3-4d77-a518-489dce95a202",
    "aa726914-e9bc-4811-8f42-9060c139d633", "c2ed454e-1b4f-47f3-9e6c-4f227c3ba924",
    "55447a9d-b19a-4037-b6b5-68084b62aa10", "02bf553f-9a9d-44ad-bf9d-55fe7b14c6fb",
    "00b20655-5464-4caf-902a-bc41c3953ca2", "4bf61905-5424-4d54-b4fd-477a62907eae",
    "573dd931-48da-4681-83c0-bfb4330c890a", "752e8dae-a715-4632-a337-048740e23d32",
    "1de50190-80bd-4ac1-bad1-eccf5f38e23d", "c60b0e39-1437-4bb4-8940-f6da3d943adf",
    "e4bf088b-35de-4730-aab6-8f860045eb29", "07f711b6-d995-4abd-89f1-2282c85a727c",
    "a2a5e9a6-2fd5-4f98-8c2a-79d8aecda9dc", "7a7e93a2-00d4-41f2-aded-8483ad38c7f1",
    "6f7fa44f-3566-461c-a041-fff14ba0b0b4", "009f32c8-cd70-4c5b-8db7-6a7991447d85",
    "b4b4cbf6-c460-4e42-b6c3-e395220f3642", "45f4a99b-5880-4f55-8987-58e80fa62aa6",
    "7ba30dac-e98c-4e99-9ae4-e1a62d1d5722", "644ea657-afb6-4a78-9f63-24b5bbfab0b0",
    "9c41489f-af62-48b4-8bd4-7126b547a59c", "b147095e-826d-4f39-96f5-38e2e822f9a7",
    "18eb61c8-5e23-42ee-9917-cde560ae84dc", "a72a82c7-d2a2-41da-bb14-cdf717c7b665",
    "3348bfd5-c5c2-4992-a8d9-67caee449bb7", "f917c656-5311-4709-8b91-b2ddb4666f23",
    "85ae4e85-e2a0-4b01-8528-188eff0336f8", "f2c8db98-c34c-4140-a868-029bf4b557b1",
    "794f1293-774f-4fb2-9543-64dea9bc37d5", "77d954c4-88d2-4a38-818d-eb5fd7a88846",
    "9bbcc4fb-fda8-4455-a9ab-092f13fedffd", "55074c3d-5f91-4eae-a44d-e17447511c4d",
    "e1c46712-d801-421a-b8fd-61cb9573f248", "d7cdf9ba-b770-4416-9011-b7e4a14afee6",
    "e3b3636a-14c6-4256-83ee-28f5913574ed", "b50c893f-8a76-4cd2-9e03-4514fc1cbc80",
    "cf3a5398-3946-4261-803c-1838d92eaeb9", "ada8aff8-ef42-48d6-8d90-099d95f0cf39",
    "8e3178f8-8171-4b2d-90cc-9b2dce76bdf8", "5bceef50-97af-4846-9c8f-faea38d00257",
    "72c81af2-b4c1-4dd7-855d-4b38b4ec5ddd", "cbf3da38-ac71-48a5-b455-23c822c22e9a",
    "16119d33-89ee-4c7a-ab16-9841ff1bda67", "8f7c165f-3561-4dad-afe7-0af5faa5a66f",
    "8bfd27f7-e308-4eef-aace-192e6a627f93", "738bb8b2-8fbc-4b9b-82a4-cc67bbf69a57",
    "443f883d-487c-4e31-a8da-ad2c31e45040", "8fe8d139-9414-4e8b-b0fd-582047fe216d",
    "993d4e41-51b3-4c59-a4ad-b3dbd482968e", "a3fe8af3-8bcf-469a-b3d8-08bc0f071c98",
    "a97ee226-fed8-4431-a008-15a6cccf98bd", "5b18bbbd-516e-40ba-8f2a-3e77aad72230",
    "909b5f69-8e28-4b9b-81cf-c86addbfd096", "ee7f1c8c-c98d-4611-aa4e-c112e86782e6",
    "25599138-d35f-4c72-9c2f-800749cedf74", "af64af4c-5a03-4218-b9fe-440253fd51ae",
    "49ef6b14-7434-465c-8c54-d27d5b7af574", "38a46979-091e-427d-9ebc-712305e87475",
    "4ae90b8f-bbb4-47cd-a914-98e1c845cf1e", "aa01d4be-7ae3-472d-92b0-408616a9d8b9",
    "cd472d88-06e5-41bd-99eb-96ccec3ff324", "bf8a347b-5e36-4f59-ab40-8e0cbe889aee",
    "99794ff6-9199-47c8-88a4-8054c5a749a8", "f8a7e9e5-9e37-43c7-ac64-aa31ba8c9801",
    "61a7fd03-8551-4179-8b0d-f03379e23b74", "1e8f0d85-cecf-49e9-8a3f-876d96b2929f",
    "f84d8475-9304-4d09-b6c6-368fef476238", "64e59197-8e85-4bf1-8993-e46ad1cf526c",
    "f3b2a44b-c254-4fb2-a00a-6a1fc234f4e9", "bacadb12-5f94-4397-8c4a-573b3e2a5d07",
    "ac27174f-f43f-47fe-8be1-5b4b6be2f80e", "b3ad94ef-b149-4342-b125-4f0f5dee749d",
    "1bda7374-b721-4a8f-bef0-84ae09d6bf54", "2b161c43-e834-4be3-b68f-52729066dd92",
    "d10e3a64-87dd-461e-9847-53a720c98052", "74c2f75c-2379-4a64-9694-a160ed21e61f",
    "c8c0dd20-97cd-4190-8367-178e6e766248", "dd04dfc6-9ecd-4854-8025-22069d4c1bf4",
    "95778172-ab3b-49a5-8f41-52a8db03a832", "d6c7783a-d639-4290-97ee-1d85394d480c",
    "ad05ef4a-3533-4b30-b1ff-3d1faf393337", "f020e28d-97f4-409b-9cad-65cbf35cc2cd",
    "8ae20f15-d9c1-45ff-a117-77f001b6eb0a", "67e75a1f-005c-4930-86e9-2c371defa500",
    "e2611edc-59cc-4cfc-bae2-a2ed46e32763", "e779b623-ff1c-4777-8ba8-83f2df6bdb9f",
    "a97ee226-fed8-4431-a008-15a6cccf98bd", "5f86bbb9-8dea-4a0f-864d-4e774b844bc8",
    "9fc1ef9e-509c-4ba4-8809-11de1b7505aa", "2c6f539a-ff4e-4163-9ecd-2a44a4df3055",
    "f972f554-26cc-4c9c-93b0-0f58b6015de2", "03eee81f-62db-46cf-ac83-6a2368561fad",
    "3cb1d5bf-7db5-4db2-82a6-4c39f6a4441b", "a6107d5f-1e70-437b-bf6f-5b4e8caaa55b",
    "f002b543-ff02-4f15-8fb5-49d150c287e7", "d62804c8-0a2e-434d-aedb-cad3c9a9e99c",
    "734ef782-b204-4fad-8e70-7190731b8237", "f7d45c6d-e506-4826-8ffe-3f75d588d378",
    "e6d3526b-9ce9-4144-96e0-b2587b4c6a74", "77736ac8-c667-4649-b2a9-fa9e701cdafb",
    "a85b378b-2287-4cf1-a0f1-0ad53cd56f56", "01bbffec-312c-493a-bcd3-e9361ed3435d",
    "51b1b6e4-129d-41a6-bbbd-c3fab459c25f", "79676d45-ee23-469a-be96-2d79acfe6dcb",
    "38b85ee7-b89e-4fef-aab5-25ec4781c7e4", "a8a6b7b8-eeea-4faa-992d-cdbd64edda3c",
    "81dd6d73-4009-4c15-b7bc-dbb67f677538", "ca376e77-d339-4dcc-9719-b09d346eee34",
    "2ff63c9f-f1c3-4cbf-9396-d6038d1cb9c1", "917167d9-1ead-4f9b-b19d-c0afd39357b0",
    "57ac54dc-3e7a-4661-b75c-5bbc09248b83", "851a3de9-f4a4-4baf-b6ee-4c63588d643b",
    "06c83fdc-961a-4691-8f80-035117e5258d", "66dcc718-b94d-4a15-bfc5-1914b71b24ea",
    "bc7ecc33-4790-47f8-a45e-e11698d96400", "4127f84b-3bac-4b5a-bde0-380a09e93324",
    "77a63718-b3f1-415b-93f8-1240ec7b2136", "73624419-f6f1-41a7-becf-7f1c90d5b203",
    "a8d02d68-a7c3-4db4-b22c-c3a0c6f764d7", "8e52800c-e326-4be8-9b79-b84f2de9338e",
    "a878800a-df2b-4b63-b286-965bc288f3a5", "989bff0e-957b-45f9-ab40-9d9c3a5f51fc",
    "0cd9f4e6-564f-4162-ba8b-875d7dd7652d", "451fa4fd-3c6e-4368-88c1-ca75932449a3",
    "d244a4f8-69e4-45b0-8bbc-82cd5520db7e", "52044499-6262-48f0-a61a-b4f193c536f7",
    "c9c5af3a-1fed-4abf-a734-3473f89cae18", "93d2c84a-5cd0-4bb3-809f-8675e0b92457",
    "cad2eeb5-7827-4b3d-b406-e20864a71637", "1329953a-2745-4c37-b77f-e6ecd7929b1e",
    "22e65f52-2e0c-47b2-b4c9-03bdbac84584", "18af6753-2f5b-49d2-a28d-4cc1b7deaf6f",
    "e03834fe-a585-479b-a334-db8a34312341", "eee6f414-8c98-4f9f-b852-fedb647467f4",
    "cbef35b0-6874-483a-96cc-1440ac0672a5", "0f7953ed-dd15-4409-b685-bacf461990c8",
    "181e0252-d904-4e44-b53d-8b119a4c4e20", "08915a93-1ea3-4373-8ff0-72d05324537f",
    "11c89dd2-7993-4503-9ea1-bc829d66ab65", "dec8dce0-d6f0-41a7-a9de-992fc558c341",
    "e46c71d9-d38c-4539-9f95-b1a26167d5d0", "fa749dd0-d69b-43a3-b239-75b80c4307aa",
    "d7c5cc6c-224c-44cb-a0c6-ff73d930ad26", "7999c103-932b-4647-815b-711a1967f0f4",
    "43d9c329-edd4-4042-b6e1-0983c3b6748e", "8162d84e-4578-4adf-98d5-2faad56bba19",
    "2d448563-aede-4a54-a061-7382551214dd", "443c7494-aa78-4a21-b2b4-cfa3639e1346",
    "7b14bf39-719f-4971-b5b3-59e8170be17d", "d6161b42-6233-4f64-802d-62f2f50c9a10",
    "de96e43d-eb1c-4703-8058-4aab05d4abf3", "44f92120-e7ea-442c-b70e-4b4cb8515f8c",
    "47c355d3-e797-4e60-8683-639d2e8ea1a4", "4813e599-82d9-4632-9417-ea30faf8034b",
    "20c3f743-cf61-4d7a-8fcb-f422f5d12557", "fd594f7c-4a09-4394-9826-31dad479ee59",
    "ee0f1d67-96c3-4db6-a850-b107fdeedd88", "98e05a38-853d-481d-a6d5-a27e49fd7a61",
    "f4d28481-3b28-4cdb-9877-9b9d4f07e5f2", "805ad5e5-03d0-4556-b563-955a2f86af9b",
    "dad08fea-5263-4f57-a37b-c27cbe0eb9a5", "b3dd721e-6084-4413-8300-44e10d8fd3ca",
    "8740361a-d335-40b2-9c7c-50188f783682", "b3cdff28-5829-4d76-84d6-e5c717d28d07",
    "2eb32aac-2fa3-4dbc-bb2f-7dcd593be16b", "9772cab5-0a3f-41dc-8fb1-156064b204d6",
    "2417e9e6-d4f1-475a-9028-91bc6867cba4", "1b478251-122f-4f10-98f5-9794336e8ba0",
    "b85442aa-e866-4cf7-bc94-035c6b48f8ab", "b2dfadad-fd4f-44f5-ae8d-8028059c6f14",
    "41669400-5d10-4b85-bc18-d616daacc968", "c9978e53-9f35-48cb-9cad-a37602a60dc4",
    "5770eb59-b382-4867-bdcc-f304eb4f0020", "26a45f4d-f777-4e9c-9289-4620c6aeca0a",
    "4d7d38eb-0973-4bac-b7c7-99e63c40fe82", "44558c36-8211-4860-93ce-3e34f2eac2c5",
    "a850f783-f3ae-4bd7-abe2-649c890367d1", "59f28864-a02b-44ab-8ed6-5e3bbecaee58",
    "a6bedf44-cf1c-4a92-b90c-2a16bd7ca6b2", "185d0368-b27f-4681-af01-29c429eb8029",
    "141e2aa7-d6a2-4b28-a347-322f7fbe5f9c", "d4b26894-62f8-4221-8dd7-a65c7337a4b3",
    "1a251f69-6993-4ac8-98c0-8245fb48cf53", "3d1b4e68-f523-47c2-8945-c701a5e5d40f",
    "13cd74e3-5ced-4411-a39a-8f0a0feb428b", "58394a77-7aaf-4c3c-a2df-dc17882f1161",
    "9cb829e3-6472-4014-9e61-dffab64cc8f6", "7eb3a461-ef8c-4767-bf7a-911144e5c929",
    "59d555a1-a819-4f8f-ad11-ac853385d316", "79ebd00e-2745-4b89-840a-641e62fe3341",
    "18bcbf41-94d9-4fac-bccb-da7c710c9275")

  sample(dlist, 1)
}

